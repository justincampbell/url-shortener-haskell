{-# LANGUAGE AutoDeriveTypeable,StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings , NoImplicitPrelude #-}

module Shortener where




import           Prelude (($), IO , putStrLn, (++) , (.), return
                         ,show, Int, Show, succ, String)
  
import Control.Applicative ((<$>))  
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
import           Data.Map
import           Data.Typeable

data World = World { _id :: Int, _urls :: Map Token Url } deriving (Show)

deriving instance Typeable World

type Token = String
type Url = String

world :: IORef World
world = declareIORef "world" initialWorld


app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app request respond = do
    response <- responseBuilder request
    respond response


responseBuilder :: Request -> IO Response
responseBuilder request =  case pathInfo request of
  [] -> return indexHandler
  ["shorten"] -> shortenHandler request
  _ -> expandHandler request




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



expandHandler :: Request -> IO Response
expandHandler request = do
        url <- expandTokenIORef $ extractToken request
        case url of
            Nothing -> return $ responseLBS status404 [] ""
            Just url' -> return $ redirectTo $ CBS.pack url'

extractToken :: Request -> Token
extractToken request =
        case pathInfo request of
            [] -> unpack ""
            [a] -> unpack a
            _ -> ""

updateShortenIORef :: Url -> IO ()
updateShortenIORef url = modifyIORef world (shorten url)

expandTokenIORef :: Token -> IO (Maybe Url)
expandTokenIORef token = do                         
    currentWorld <- readIORef world
    return $ expand token currentWorld



indexHandler :: Response
indexHandler = redirectTo "https://github.com/justincampbell/url-shorteners"

redirectTo :: CBS.ByteString -> Response
redirectTo url = responseLBS status302 [("Location", url)] ""

shortenHandler :: Request -> IO Response
shortenHandler request = do
    let
      url = extractUrl request
      headers = []
    shortenResult <-  updateShortenIORef url
    token <- lastToken <$>
             readIORef world
    let             
     body = encode $ "/" ++ token
    return $ responseLBS status201 headers body 

extractUrl :: Request -> Url
extractUrl request = case url of
                         Just url' -> CBS.unpack $ fromJust url'
                         Nothing -> ""
                     where url = P.lookup "url" $ queryString request


