{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}

module LobbyType(
  Lobby(..)
) where

import Data.Aeson
import Data.Aeson.TH
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import GHC.TypeLits

data Lobby = Lobby
  { userId        :: UUID
  , users         :: [String]
  } deriving Generic
