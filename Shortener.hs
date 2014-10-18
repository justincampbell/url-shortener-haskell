module Shortener where

import Data.Map

data World = World { _id :: Int, _urls :: Map Token Url } deriving (Show)

type Token = String
type Url = String

initialWorld :: World
initialWorld = World 0 empty

shorten :: World -> Url -> (World, Token)
shorten world url = (newWorld, token) where
        nextId = succ $ _id world
        newWorld = World nextId empty
        token = show nextId
