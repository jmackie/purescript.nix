{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module PureScript
  ( ModuleName(..)
  , parseModuleName
  ) where

import           Prelude

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Encoding  as Aeson.Encoding
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char            as Char
import qualified Data.Text            as Text

import           Control.Applicative  ((<|>))
import           Data.Functor         (($>))
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Text            (Text)


newtype ModuleName = ModuleName { unModuleName :: NonEmpty Text }
  deriving stock (Eq, Ord)

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
    pn  <- properName
    pns <- (Attoparsec.char '.' *>
            Attoparsec.sepBy properName (Attoparsec.char '.'))
        <|> pure []

    pure $ ModuleName (pn :| pns)

  properName :: Attoparsec.Parser Text
  properName = do
    c  <- Attoparsec.satisfy Char.isUpper
    cs <- Attoparsec.many' (Attoparsec.letter <|> Attoparsec.digit)
    pure $ Text.pack (c : cs)

  lexeme :: Attoparsec.Parser a -> Attoparsec.Parser a
  lexeme = (<* spaceConsumer)

  spaceConsumer :: Attoparsec.Parser ()
  spaceConsumer =
    Attoparsec.choice
      [ lineComment *> Attoparsec.skipSpace *> spaceConsumer
      , multilineComment *> Attoparsec.skipSpace *> spaceConsumer
      , Attoparsec.skipSpace
      ]

  lineComment :: Attoparsec.Parser ()
  lineComment =
    Attoparsec.string "--" *>
    Attoparsec.takeTill Attoparsec.isEndOfLine $> ()

  multilineComment :: Attoparsec.Parser ()
  multilineComment =
    Attoparsec.string "{-" *>
    Attoparsec.manyTill Attoparsec.anyChar (Attoparsec.string "-}") $> ()
