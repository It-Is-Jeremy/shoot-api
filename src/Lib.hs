{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import GHC.TypeLits
import Control.Monad.IO.Class (liftIO)
import Data.UUID
import Data.UUID.V4

import SimpleApi

type LobbyId = UUID

type API = SimpleAPI "lobby" Lobby LobbyId

data Lobby = Lobby
  { id        :: UUID
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Lobby)


simpleServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

lobbyServer :: Server (SimpleAPI "lobby" Lobby LobbyId)
lobbyServer = simpleServer
  (return [])
  (\lobbyId ->
      if lobbyId == nil
      then do
         id <- liftIO (nextRandom)
         return $ (Lobby id)
      else
        return $ (Lobby lobbyId)
  )
  (\_lobby -> return NoContent)

startApp :: IO ()
startApp = run 8080 app

api :: Proxy API
api = Proxy

app :: Application
app = serve api lobbyServer
