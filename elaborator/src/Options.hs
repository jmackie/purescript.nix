{-# LANGUAGE RankNTypes #-}
module Options
  ( Options(..)
  , exec
  ) where
import           Prelude

import qualified Options.Applicative    as Opt
import qualified Path

import           Control.Applicative    ((<**>), (<|>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Path                   (Path)


type PathOption a = Either (Path Path.Abs a) (Path Path.Rel a)


fileOption :: Opt.ReadM (PathOption Path.File)
fileOption = Opt.maybeReader $ \str ->
  fmap Left (Path.parseAbsFile str) <|>
  fmap Right (Path.parseRelFile str)


-- | Command line options.
newtype Options = Options
  { optPackageSet  :: PathOption Path.File
  }


exec :: MonadIO m => m Options
exec = liftIO (Opt.execParser parserInfo)
  where
  parserInfo :: Opt.ParserInfo Options
  parserInfo =
    Opt.info (parser <**> Opt.helper) $ mconcat
      [ Opt.fullDesc
      , Opt.progDesc "Print a greeting for TARGET"
      , Opt.header "hello - a test for optparse-applicative"
      ]

  parser :: Opt.Parser Options
  parser = pure Options
    <*> packageSet


-- OPTION PARSERS


packageSet :: Opt.Parser (PathOption Path.File)
packageSet = flag <|> positional
  where
    flag =
      Opt.option fileOption $ mconcat
        [ Opt.long "package-set"
        , Opt.metavar "FILE"
        , Opt.help "package set json file"
        ]

    positional =
      Opt.argument fileOption (Opt.metavar "packages.json")
