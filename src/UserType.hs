{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
module UserType(
  User(..)
) where

import Data.Aeson
import Data.Aeson.TH
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import GHC.TypeLits

data User = User
  { lobbyId   :: UUID
  , username  :: String
  } deriving Generic