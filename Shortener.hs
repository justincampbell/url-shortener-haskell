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
        token = show nextId
        newUrls = insert token url $ _urls world
        newWorld = World nextId newUrls

expand :: World -> Token -> Maybe Url
expand world token = Data.Map.lookup token $ _urls world
