module Main where

import Data.Map
import Shortener
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "shorten" $ do
            it "returns the first token" $
                snd (shorten url initialWorld) `shouldBe` "1"
            it "increments the token" $
                snd (shorten url (fst (shorten url initialWorld))) `shouldBe` "2"
        describe "expand" $ do
            it "returns nothing for an empty world" $
                expand initialWorld "foo" `shouldBe` Nothing
            it "returns a previously shortened url" $
                let world = World 1 (singleton "1" url) in
                    expand world "1" `shouldBe` Just url
        describe "shorten and expand" $
            it "stores and retrieves a url by token" $
                let (newWorld, token) = shorten url initialWorld in
                  expand newWorld token `shouldBe` Just url
        where url = "http://justincampbell.me"
