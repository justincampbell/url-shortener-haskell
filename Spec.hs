module Main where

import Data.Map
import Shortener
import Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "shorten" $ do
            it "returns the first token" $
                snd (shorten initialWorld url) `shouldBe` "1"
            it "increments the token" $
                snd (shorten (fst (shorten initialWorld url)) url) `shouldBe` "2"
        describe "expand" $ do
            it "returns nothing for an empty world" $
                expand initialWorld "foo" `shouldBe` Nothing
            it "returns a previously shortened url" $
                let world = World 1 (singleton "1" url) in
                    expand world "1" `shouldBe` Just url
        describe "shorten and expand" $ do
            it "stores and retrieves a url by token" $
                let (newWorld, token) = (shorten initialWorld url) in
                  expand newWorld token `shouldBe` Just url
        where url = "http://justincampbell.me"
