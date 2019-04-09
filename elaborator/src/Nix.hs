{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Not currently in use.
--
module Nix where

import           Prelude

import qualified Data.Text.Prettyprint.Doc             as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc.Render

import           Data.List.NonEmpty                    (NonEmpty (..))
import           Data.String                           (IsString)
import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc             (Doc, (<+>))


newtype Var = Var Text
  deriving newtype (IsString)


data Expr r
  = Abs (Param r) r
  | RecSet [Binding r]
  | Sym Var
  deriving (Functor)


data Param r
  = Param Var
  | ParamSet (ParamSet r)
  deriving ( Functor )


type ParamSet r = [(Var, Maybe r)]


data Binding r
  = NamedBinding AttrPath r
  deriving ( Functor )


type AttrPath = NonEmpty KeyName


newtype KeyName = StaticKey Var


renderNix :: Fix Expr -> Text
renderNix =
  Doc.Render.renderStrict .
  Doc.layoutPretty Doc.defaultLayoutOptions .
  prettyExpr

  where
    prettyExpr :: Fix Expr -> Doc a
    prettyExpr = cata $ \case
      Sym var -> prettyVar var
      Abs param body -> prettyParam param <> ":" <+> body
      RecSet bindings ->
        "rec" <+> Doc.braces (Doc.vsep (prettyBinding <$> bindings))

    prettyParam :: Param (Doc a) -> Doc a
    prettyParam = \case
      Param var -> prettyVar var
      ParamSet ps -> prettyParamSet ps

    prettyParamSet :: ParamSet (Doc a) -> Doc a
    prettyParamSet =
      Doc.enclose "{ " " }" . foldMap (\( var, def ) ->
        case def of
          Nothing -> prettyVar var
          Just d  -> prettyVar var <+> "?" <+> d)

    prettyBinding :: Binding (Doc a) -> Doc a
    prettyBinding = \case
      NamedBinding (a :| as) doc ->
            Doc.hsep (Doc.punctuate "." (prettyKeyName <$> a : as))
            <+> "="
            <+> doc
            <> ";"

    prettyKeyName :: KeyName -> Doc a
    prettyKeyName (StaticKey var) = prettyVar var

    prettyVar :: Var -> Doc a
    prettyVar (Var var) = Doc.pretty var


newtype Fix f = Fix { unFix :: f (Fix f) }


cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
