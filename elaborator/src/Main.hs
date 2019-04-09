{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Prelude

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Data.Error               as Error
import qualified Data.Map                 as Map
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text.IO
import qualified Options
import qualified Path
import qualified Path.IO
import qualified System.IO                as IO
import qualified System.Process           as Process

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad.Script     (Script, runIO, runScript)
import           Data.ByteString          (ByteString)
import           Data.Either              (partitionEithers)
import           Data.Error               ()
import           Data.Foldable            (for_)
import           Data.Function            ((&))
import           Data.Map                 (Map)
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Options                  (Options (..))
import           Path                     (Path, (</>))
import           PscPackage               (PackageInfo (..), PackageName (..),
                                           PackageSet)
import           PureScript               (ModuleName, parseModuleName)
import           System.Exit              (ExitCode (..))


type ExtendedPackageSet = Map PackageName ExtendedPackageInfo

data ExtendedPackageInfo
  = ExtendedPackageInfo
      { repo         :: Text
      , version      :: Text
      , dependencies :: [PackageName]
      , sha256       :: Text
      , sources      :: Map ModuleName (Path Path.Rel Path.File)
      , foreigns     :: [Path Path.Rel Path.File]
      }
  deriving (Generic, Aeson.ToJSON)


-- | Format like @psc-package@ does.
formatExtendedPackageSet
  :: ExtendedPackageSet -> ByteString
formatExtendedPackageSet
  = ByteString.Lazy.toStrict
  . Aeson.Pretty.encodePretty' prettyConfig
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


main :: IO ()
main = do
  options <- Options.exec
  result <- Path.IO.withSystemTempDir "purescript-nix-XXX"
            (runScript . script options)
  case result of
    Left exception -> IO.hPrint IO.stderr exception
    Right ps ->
      ByteString.hPutStr IO.stdout $
        formatExtendedPackageSet ps


script
  :: Options
  -> Path Path.Abs Path.Dir
  -- ^ Temporary directory to work in
  -> Script ExtendedPackageSet
script Options{..} tempDir = do
  ps  <- Error.describe "reading package set file" $
         normalizeFile optPackageSet >>=
         readPackageSet

  unhelpfulIO $ do
    -- TODO: Optionally throttle this with a semaphore?
    results <- forConcurrently (Map.toList ps) $ \(name, info) -> do
      result <- runScript (extendPackageInfo tempDir name info)
      case result of
        Left err           -> pure $ Left (name, err)
        Right extendedInfo -> pure $ Right (name, extendedInfo)

    let (errs, kvs) = partitionEithers results
    for_ errs $ \(PackageName name, err) ->
      Text.IO.putStrLn ("Error downloading " <> name <> ": " <> Text.pack (show err))
    pure $ Map.fromList kvs


readPackageSet :: Path b Path.File -> Script PackageSet
readPackageSet path = do
  let notExistsError = Error.raiseString (Path.toFilePath path <> " doesn't exist")
  bytes <- readBytes path >>= maybe notExistsError pure
  case Aeson.eitherDecodeStrict bytes of
    Left err -> Error.describe "error decoding json" $
                Error.raiseString err
    Right ps -> pure ps


extendPackageInfo
  :: Path Path.Abs Path.Dir
  -> PackageName
  -> PackageInfo
  -> Script ExtendedPackageInfo
extendPackageInfo tempDir (PackageName name) PackageInfo{..} = do
  repoDir <-
    unhelpfulIO $
      Path.IO.createTempDir tempDir (Text.unpack name <> "-XXX")

  gitCloneShallow pkgRepo pkgVersion repoDir >>= \case
    (ExitSuccess, _, _) -> pure ()

    (ExitFailure _, _, stderr) ->
      Error.describe ("cloning " <> pkgRepo) $
      Error.raiseString ("\n\n" <> Text.unpack (Text.unlines stderr))

  -- Replicating what @nix-prefetch-git@ does here
  removeDotGit repoDir
  sha256 <- nixHash repoDir >>= \case
    (ExitSuccess, [hash], _) -> pure hash

    (ExitSuccess, wat, _) ->
      Error.describe ("hashing " <> pkgRepo) $
      Error.raiseString ("unexpected nix-hash output" <> Text.unpack (Text.unlines wat))

    (ExitFailure _, _, stderr) ->
      Error.describe ("hashing " <> pkgRepo) $
      Error.raiseString ("\n\n" <> Text.unpack (Text.unlines stderr))

  -- I think we can reasonably assume that all package source files
  -- will live under a top-level src directory
  let src = [Path.reldir|src|]
  (absSources, absForeigns) <- walkSources (repoDir </> src)

  sources  <- traverse (unhelpfulIO . Path.IO.makeRelative repoDir) absSources
  foreigns <- traverse (unhelpfulIO . Path.IO.makeRelative repoDir) absForeigns

  -- Progress feedback
  pure ExtendedPackageInfo
    { repo = pkgRepo
    , version = pkgVersion
    , dependencies = pkgDependencies
    , sha256 = sha256
    , sources = sources
    , foreigns = foreigns
    }


walkSources
  :: Path b Path.Dir
  -> Script (Map ModuleName (Path Path.Abs Path.File), [Path Path.Abs Path.File] )
walkSources root =
  helpfulIO "walk purescript source files" $
    Path.IO.walkDirAccum (Just descend) walk root
  where
  descend _dir _subdirs _files = pure (Path.IO.WalkExclude [])

  walk _dir _subdirs = (fmap (mconcat . catMaybes) .) . traverse $
    \file ->
      case Path.fileExtension file of
        ".purs" -> do
           contents <- Text.IO.readFile (Path.fromAbsFile file)
           -- TODO: Handle this error case
           let mn = either undefined id (parseModuleName contents)
           pure $ Just (Map.singleton mn file, [])

        ".js" ->
           pure $ Just (mempty, [file])

        _ -> pure Nothing


nixHash :: Path b Path.Dir -> Script (ExitCode, [Text], [Text])
nixHash dir =
  sh "nix-hash" [ "--type"
                , "sha256"
                , "--base32"
                , Text.pack (Path.toFilePath dir)
                ]


-- | This is the same git command run by @psc-package@.
gitCloneShallow
  :: Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Path b Path.Dir
  -- ^ target directory
  -> Script (ExitCode, [Text], [Text])
  -- ^ exit code, stdout lines, stderr lines
gitCloneShallow from ref into =
  git [ "clone"
      , "-q"
      , "-c"
      , "advice.detachedHead=false"
      , "--depth"
      , "1"
      , "-b"
      , ref
      , from
      , Text.pack (Path.toFilePath into)
      ]


removeDotGit :: Path b Path.Dir -> Script ()
removeDotGit repo =
  helpfulIO "" $
  Path.IO.removeDirRecur (repo </> [Path.reldir|.git|])


git :: [Text] -> Script (ExitCode,[Text],[Text])
git = sh "git"


-- IO HELPERS


sh :: Text -> [Text] -> Script (ExitCode,[Text],[Text])
sh cmd args = helpfulIO errorMsg $ do
  (exitCode,stdout,stderr) <-
    Process.readProcessWithExitCode
      (Text.unpack cmd)
      (Text.unpack <$> args)
      "" -- stdin

  pure
    ( exitCode
    , Text.lines (Text.pack stdout)
    , Text.lines (Text.pack stderr)
    )

  where errorMsg = "run " <> cmd <> " " <> Text.unwords args


readBytes :: Path b Path.File -> Script (Maybe ByteString)
readBytes path = do
  let filePath = Path.toFilePath path
  exists <- Path.IO.doesFileExist path & unhelpfulIO
  if not exists
     then pure Nothing
     else do
        bytes <- ByteString.readFile filePath & unhelpfulIO
        pure (Just bytes)


normalizeFile
  :: Either (Path Path.Abs Path.File) (Path Path.Rel Path.File)
  -> Script (Path Path.Abs Path.File)
normalizeFile (Left absFile) = pure absFile
normalizeFile (Right relFile) = Path.IO.makeAbsolute relFile & helpfulIO errorMsg
  where errorMsg = "make " <> renderPath relFile <> " absolute"


normalizeDir
  :: Either (Path Path.Abs Path.Dir) (Path Path.Rel Path.Dir)
  -> Script (Path Path.Abs Path.Dir)
normalizeDir (Left absDir) = pure absDir
normalizeDir (Right relDir) = Path.IO.makeAbsolute relDir & helpfulIO errorMsg
  where errorMsg = "make " <> renderPath relDir <> " absolute"


helpfulIO :: Text -> IO a -> Script a
helpfulIO what = runIO (Error.raiseWithContext what)


unhelpfulIO :: IO a -> Script a
unhelpfulIO = runIO Error.raise


-- MISC HELPERS


renderPath :: Path b t -> Text
renderPath = Text.pack . Path.toFilePath
