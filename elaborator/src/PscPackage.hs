{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Stuff borrowed from https://github.com/purescript/psc-package
--
module PscPackage where

import           Prelude

import qualified Data.Aeson   as Aeson
import qualified Data.Char    as Char
import qualified Data.List    as List

import           Data.Map     (Map)
import           Data.Proxy   (Proxy (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic, Rep)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


type PackageSet = Map PackageName PackageInfo


newtype PackageName = PackageName Text
  deriving newtype (Eq, Ord, Aeson.FromJSON, Aeson.FromJSONKey, Aeson.ToJSON, Aeson.ToJSONKey)


data PackageInfo
  = PackageInfo
      { pkgRepo         :: Text
      , pkgVersion      :: Text
      , pkgDependencies :: [PackageName]
      }
  deriving (Generic)
  deriving Aeson.FromJSON via (StripPrefix "pkg" PackageInfo)


-- The below really isn't necessary, but I just wanted to 
-- use DerivingVia...no regrets.


newtype StripPrefix (s :: Symbol) a = StripPrefix a
  deriving newtype (Generic)


instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  , KnownSymbol s
  ) => Aeson.FromJSON (StripPrefix s a) where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions
      { Aeson.fieldLabelModifier =
          List.stripPrefix (symbolVal (Proxy @s)) >>= \stripped def ->
            case stripped of
              Nothing       -> def
              Just []       -> def
              Just (c : cs) -> Char.toLower c : cs
      }
