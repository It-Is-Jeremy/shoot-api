{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /lobby" $ do
        it "responds with 200" $ do
            get "/lobby" `shouldRespondWith` 200
    describe "GET /user" $ do
            it "responds with 200" $ do
                get "/user" `shouldRespondWith` 200