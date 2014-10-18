module Main where

import Shortener
import Test.Hspec

main :: IO ()
main = do
    hspec $
        describe "shorten" $ do
            it "returns the first token" $
                snd (shorten world url) `shouldBe` "1"
            it "increments the token" $
               snd (shorten (fst (shorten world url)) url) `shouldBe` "2"
        where world = initialWorld
              url = "foo"
