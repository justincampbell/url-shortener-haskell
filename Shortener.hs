{-# LANGUAGE AutoDeriveTypeable,StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings , NoImplicitPrelude #-}

module Shortener where




import           Prelude (($), IO , putStrLn, (++) , (.)
                         ,show, Int, Show, succ, String)

import qualified Prelude as P
import           Data.Binary (encode)
import qualified Data.ByteString.Char8 as CBS
import           Data.Global (declareIORef)
import           Data.IORef (IORef, readIORef, modifyIORef)
import           Data.Maybe (Maybe (..) , fromJust)
import           Data.Text (unpack, Text)
import Network.HTTP.Types (status302,
                           status201,
                           status404)
import           Network.Wai (Request, Response,Application, pathInfo, responseLBS, queryString, ResponseReceived)
import           Network.Wai.Handler.Warp (run)

import           System.IO.Unsafe



import           Data.Map
import           Data.Typeable

data World = World { _id :: Int, _urls :: Map Token Url } deriving (Show)

deriving instance Typeable World

type Token = String
type Url = String

world :: IORef World
world = declareIORef "world" initialWorld


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


responseBuilder request =  case pathInfo request of
  [] -> indexHandler
  ["shorten"] -> shortenHandler request
  _ -> expandHandler request


expandHandler :: Request -> Response
expandHandler request =
        case url of
            Nothing -> responseLBS status404 [] ""
            Just url' -> redirectTo $ CBS.pack url'
        where url = unsafeExpand $ extractToken request

extractToken :: Request -> Token
extractToken request =
        case pathInfo request of
            [] -> unpack ""
            [a] -> unpack a
            _ -> ""

unsafeShorten :: Url -> ()
unsafeShorten url = unsafePerformIO $ modifyIORef world (shorten url)

unsafeExpand :: Token -> Maybe Url
unsafeExpand token = expand token currentWorld where
    currentWorld = unsafePerformIO $ readIORef world



indexHandler :: Response
indexHandler = redirectTo "https://github.com/justincampbell/url-shorteners"

redirectTo :: CBS.ByteString -> Response
redirectTo url = responseLBS status302 [("Location", url)] ""

shortenHandler :: Request -> Response
shortenHandler request = responseLBS status headers body where
    url = extractUrl request
    shortenResult = unsafeShorten url
    status = case shortenResult of () -> status201
    headers = []
    token = lastToken $ unsafePerformIO $ readIORef world
    body = encode $ "/" ++ token

extractUrl :: Request -> Url
extractUrl request = case url of
                         Just url' -> CBS.unpack $ fromJust url'
                         Nothing -> ""
                     where url = P.lookup "url" $ queryString request


