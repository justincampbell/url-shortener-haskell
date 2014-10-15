module Shortener where

type Url = String

data State = State { _id :: Int }

shorten :: State -> Url -> (State, String)
shorten state _ = (newstate, token) where
    newstate = State $ inc (_id state)
    token = show (_id newstate)

inc :: Int -> Int
inc n = n + 1
