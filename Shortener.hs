module Shortener where

type Token = String
type Url = String

data State = State { _id :: Int }

shorten :: State -> Url -> (State, Token)
shorten state _ = (newstate, token) where
    newstate = State $ succ (_id state)
    token = show (_id newstate)
