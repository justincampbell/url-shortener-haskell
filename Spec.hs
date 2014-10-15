module Main where

import Shortener
import Test.Hspec

main :: IO ()
main = hspec $
        describe "shorten" $ do
            it "returns the first token" $
                snd (shorten emptystate url) `shouldBe` "1"
            it "increments every token" $
                let (newstate, token) = shorten emptystate url in do
                    token `shouldBe` "1"
                    snd (shorten newstate url) `shouldBe` "2"
            where emptystate = State 0
                  url = "foo"
