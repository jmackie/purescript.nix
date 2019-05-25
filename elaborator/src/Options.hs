{-# LANGUAGE RankNTypes #-}
module Options
  ( Options(..)
  , exec
  ) where
import           Prelude

import qualified Options.Applicative             as Opt
import qualified Options.Applicative.Help.Pretty as Doc
import qualified Path

import           Control.Applicative             ((<**>), (<|>))
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Path                            (Path)


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
      , Opt.headerDoc . pure $
          Doc.cyan (Doc.text "PureScript package set elaborator")
      , Opt.progDescDoc . pure $
          Doc.hardline <>
          Doc.cyan (Doc.text "Adds some extra fields to the standard \
                             \PureScript package set.") <>
          Doc.hardline <>
          Doc.cyan (Doc.text "If no input file is specified it will be read from " <> Doc.bold (Doc.text "stdin.")) <>
          Doc.hardline <>
          Doc.cyan (Doc.text "The result is written to " <> Doc.bold (Doc.text "stdout."))
      , Opt.footerDoc . pure $
          Doc.text "Example:" <>
          Doc.hardline <>
          Doc.hardline <>
          Doc.indent 2 (Doc.green (Doc.text "elaborate-purescript-packages <(curl https://raw.githubusercontent.com/purescript/package-sets/master/packages.json)"))
      ]

  parser :: Opt.Parser Options
  parser =
    Options <$> packageSet


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
