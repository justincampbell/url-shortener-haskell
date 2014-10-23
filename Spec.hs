module Main where

import Data.Map
import Shortener
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "shorten" $ do
            it "updates the world" $
                lastToken (shorten url initialWorld) `shouldBe` "1"
            it "increments the token" $
                lastToken (shorten url (shorten url initialWorld)) `shouldBe` "2"
        describe "expand" $ do
            it "returns nothing for an empty world" $
                expand initialWorld "foo" `shouldBe` Nothing
            it "returns a previously shortened url" $
                let world = World 1 (singleton "1" url) in
                    expand world "1" `shouldBe` Just url
        describe "shorten and expand" $
            it "stores and retrieves a url by token" $
                let newWorld = shorten url initialWorld
                    token = lastToken newWorld in
                        expand newWorld token `shouldBe` Just url
        where url = "http://justincampbell.me"
