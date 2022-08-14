{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
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

import UserType
import LobbyType
import SimpleApi

type LobbyId = UUID
type UserId = UUID


instance FromJSON User
instance ToJSON   User


instance FromJSON Lobby
instance ToJSON   Lobby

type FactoringAPI =
  "x" :> Capture "x" Int :>
      (    QueryParam "y" Int :> Get '[JSON] Int
      :<|>                       Post '[JSON] Int
      )

factoringServer :: Server FactoringAPI
factoringServer x = getXY :<|> postX
  where getXY Nothing  = return x
        getXY (Just y) = return (x + y)
        postX = return (x - 1)

type API = FactoringAPI
     :<|> SimpleAPI "lobby" Lobby LobbyId
     :<|> SimpleAPI "user" User UserId



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
  (\requestId ->
      if requestId == nil
      then do
         id <- liftIO (nextRandom)
         return $ (Lobby id [])
      else
        return $ (Lobby requestId [])
  )
  (\_lobby -> return NoContent)


userServer :: Server (SimpleAPI "user" User UserId)
userServer = simpleServer
  (return [])
  (\userId ->
      if userId == nil
      then do
         id <- liftIO (nextRandom)
         return $ (User id "John")
      else
        return $ (User userId "James")
  )
  (\_user -> return NoContent)

startApp :: IO ()
startApp = run 8080 app

api :: Proxy API
api = Proxy

app :: Application
app = serve api $
        factoringServer :<|> lobbyServer :<|> userServer
