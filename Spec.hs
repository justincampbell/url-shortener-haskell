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
                expand "foo" initialWorld `shouldBe` Nothing
            it "returns a previously shortened url" $
                let world = World 1 (singleton "1" url) in
                    expand "1" world `shouldBe` Just url
        describe "shorten and expand" $
            it "stores and retrieves a url by token" $
                let newWorld = shorten url initialWorld
                    token = lastToken newWorld in
                        expand token newWorld `shouldBe` Just url
        where url = "http://justincampbell.me"
