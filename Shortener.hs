module Shortener where

import Data.Map

data World = World { _id :: Int, _urls :: Map Token Url } deriving (Show)

type Token = String
type Url = String

initialWorld :: World
initialWorld = World 0 empty

shorten :: Url -> World -> World
shorten url world = newWorld where
        nextId = succ $ _id world
        token = show nextId
        newUrls = insert token url $ _urls world
        newWorld = World nextId newUrls

lastToken :: World -> Token
lastToken = show . _id

expand :: Token -> World -> Maybe Url
expand token world = Data.Map.lookup token $ _urls world
