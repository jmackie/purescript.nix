{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Prelude 

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Kesha
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

import Control.Applicative ((<|>))
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Functor ((<&>), ($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.Exit (ExitCode(..), die)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  bytes <- BSL.getContents
  case Aeson.eitherDecode @(PackageSet PackageInfo) bytes of
    Left err -> die ("error decoding stdin: " <> err)
    Right packageSet -> 
      Temp.withSystemTempDirectory "elaborator" $ \tempDir -> do 
        logLock <- newQSem 1
        results <- 
          mapConcurrently 20
            (\(name, info) -> 
                fmap (name,) <$> extendPackageInfo (concurrentLog logLock) tempDir name info
            )
            (Map.toList packageSet)

        case runEithers results of
          Left errors -> 
            traverse_ print errors

          Right kvs -> do
            BSL.putStr $ formatExtendedPackageSet (Map.fromList kvs)
            putStrLn ""

mapConcurrently :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrently j f ta = do
  sem <- newQSem j
  Async.mapConcurrently (bracket_ (waitQSem sem) (signalQSem sem) . f) ta

concurrentLog :: QSem -> String -> IO ()
concurrentLog lock string = 
  bracket_ (waitQSem lock) (signalQSem lock) (hPutStrLn stderr string)

-- | 
-- Format like @psc-package@ does.
formatExtendedPackageSet :: PackageSet ExtendedPackageInfo -> BSL.ByteString
formatExtendedPackageSet = 
  Aeson.Pretty.encodePretty' prettyConfig
  where
  prettyConfig :: Aeson.Pretty.Config
  prettyConfig =
    Aeson.Pretty.defConfig
      { Aeson.Pretty.confCompare = Aeson.Pretty.keyOrder
          [ "dependencies"
          , "repo"
          , "version"
          , "sha256"
          , "sources"
          , "foreigns"
          ] <> compare
      , Aeson.Pretty.confIndent = Aeson.Pretty.Spaces 2
      , Aeson.Pretty.confTrailingNewline = True
      }

type PackageSet info
  = Map PackageName info

type PackageName 
  = String

data PackageInfo
  = PackageInfo
      { repo :: String
      , version :: String
      , dependencies :: [PackageName]
      }
  deriving (Generic, Show, Aeson.FromJSON)

data ExtendedPackageInfo
  = ExtendedPackageInfo
      { repo :: String
      , version :: String
      , dependencies :: [PackageName]
      , sha256 :: String
      , sources :: Map ModuleName FilePath
      , foreigns :: [FilePath]
      }
  deriving (Generic, Show, Aeson.ToJSON)

extendPackageInfo 
  :: (String -> IO ())  -- ^ logging function
  -> FilePath 
  -> PackageName 
  -> PackageInfo 
  -> IO (Either ExtendError ExtendedPackageInfo)
extendPackageInfo log' tempDir name PackageInfo { repo, version, dependencies } = 
  Temp.withTempDirectory tempDir name $ \repoDir -> do

    log' ("Cloning " <> repo) -- " into " <> repoDir)
    cloneResult <- cloneShallow repo version repoDir
    case cloneResult of
      (ExitFailure code, _out, err) ->
        pure $ Left (GitCloneError code err)

      (ExitSuccess, _out, _err) -> do
        (purs, js) <- 
          walkFiles (repoDir </> "src") ([], []) $ 
            \(purs, js) path -> 
              case FilePath.takeExtension path of
                ".js" -> (purs, path : js)
                ".purs" -> (path : purs, js)
                _ -> (purs, js) -- NoOp

        parseResults <- 
          for purs $ \path ->
            first (path,) . parseModuleName <$> Text.IO.readFile path 

        case runEithers parseResults of
          Left errors -> pure $ Left (ParseErrors errors)

          Right moduleNames -> do
            Directory.removeDirectoryRecursive (repoDir </> ".git")
            Kesha.hash repoDir <&> \case
              Left err -> Left (HashError err)

              Right sha256 ->
                let trimRepoDir = drop (length repoDir + 1) in
                Right ExtendedPackageInfo
                  { repo 
                  , version 
                  , dependencies 
                  , sha256 = ASCII.unpack sha256
                  , sources = Map.fromList (zipWith (\mn p -> (mn, trimRepoDir p)) moduleNames purs)
                  , foreigns = fmap trimRepoDir js
                  }

-- |
-- Things that go wrong when extending package info.
data ExtendError 
  = GitCloneError Int String
  | HashError String
  | ParseErrors [(FilePath, String)]
  deriving (Show)

walkFiles :: FilePath -> a -> (a -> FilePath -> a) -> IO a
walkFiles path accum f = do
  exists <- Directory.doesPathExist path
  if exists then do
    isFile <- Directory.doesFileExist path
    isDir <- Directory.doesDirectoryExist path
    if isFile then 
      --path' <- Directory.makeAbsolute path
      pure (f accum path)
    else if isDir then do
      entries <- Directory.listDirectory path
      foldM (\accum' path' -> walkFiles (path </> path') accum' f) accum entries
    else pure accum
  else pure accum

-- |
-- Same command used by @psc-package@.
cloneShallow :: String -> String -> String -> IO (ExitCode, String, String)
cloneShallow from ref into = 
  Process.readProcessWithExitCode "git"
    [ "clone"
    , "-q"
    , "-c", "advice.detachedHead=false"
    , "--depth", "1"
    , "-b", ref
    , from
    , into
    ] 
    mempty -- stdin

-- |
-- PureScript module name.
newtype ModuleName = ModuleName { unModuleName :: NonEmpty Text }
  deriving stock (Show, Eq, Ord)

instance Aeson.ToJSON ModuleName where
  toJSON = Aeson.toJSON . renderModuleName

instance Aeson.ToJSONKey ModuleName where
  toJSONKey = Aeson.ToJSONKeyText renderModuleName
    (Aeson.Encoding.text . renderModuleName)

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName (mn :| mns)) = Text.intercalate "." (mn : mns)

parseModuleName :: Text -> Either String ModuleName
parseModuleName = Attoparsec.parseOnly $ do
  _ <- spaceConsumer
  _ <- lexeme (Attoparsec.string "module")
  moduleName

  where
  moduleName :: Attoparsec.Parser ModuleName
  moduleName = do
    pn <- properName
    pns <- (Attoparsec.char '.' *>
            Attoparsec.sepBy properName (Attoparsec.char '.'))
        <|> pure []

    pure $ ModuleName (pn :| pns)

  properName :: Attoparsec.Parser Text
  properName = do
    c <- Attoparsec.satisfy Char.isUpper
    cs <- Attoparsec.many' (Attoparsec.letter <|> Attoparsec.digit)
    pure $ Text.pack (c : cs)

  lexeme :: Attoparsec.Parser a -> Attoparsec.Parser a
  lexeme = (<* spaceConsumer)

  spaceConsumer :: Attoparsec.Parser ()
  spaceConsumer =
    Attoparsec.skipSpace *> 
      Attoparsec.choice
        [ lineComment *> spaceConsumer
        , multilineComment *> spaceConsumer
        , pure ()
        ]

  lineComment :: Attoparsec.Parser ()
  lineComment =
    Attoparsec.string "--" *>
    Attoparsec.takeTill Attoparsec.isEndOfLine $> ()

  multilineComment :: Attoparsec.Parser ()
  multilineComment =
    Attoparsec.string "{-" *>
    Attoparsec.manyTill Attoparsec.anyChar (Attoparsec.string "-}") $> ()

runEithers :: [Either e a] -> Either [e] [a]
runEithers eithers = 
  case partitionEithers eithers of
    ([], as) -> Right as
    (es, _) -> Left es
