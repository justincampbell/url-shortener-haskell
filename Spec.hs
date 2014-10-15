module Main where

import Shortener
import Test.Hspec

main :: IO ()
main = hspec $
        describe "shorten" $
            it "returns the first id" $
                shorten s url `shouldBe` "1"
            where s = State 0
                  url = "foo"
