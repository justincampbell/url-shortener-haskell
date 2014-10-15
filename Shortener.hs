module Shortener where

type Url = String

data State = State { _id :: Int }

shorten :: State -> Url -> String
shorten state _ = show $ inc (_id state)

inc :: Int -> Int
inc n = n + 1
