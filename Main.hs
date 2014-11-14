{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Binary (encode)
import qualified Data.ByteString.Char8 as Char
import Data.Global (declareIORef)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Maybe (fromJust)
import Data.Text as Text
import Network.HTTP.Types (status201, status302, status404)
import Network.Wai (Request, Response, pathInfo, responseLBS, queryString, ResponseReceived)
import Network.Wai.Handler.Warp (run)

import Shortener

world :: IORef World
world = declareIORef "world" initialWorld

main :: IO ()
main = do
        putStrLn "listening on 8080"
        run 8080 app

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app request respond = do
    response <- dispatch request
    respond response

dispatch :: Request -> IO Response
dispatch request =  case pathInfo request of
  [] -> return indexHandler
  ["shorten"] -> shortenHandler request
  _ -> expandHandler request

indexHandler :: Response
indexHandler = redirectTo "https://github.com/justincampbell/url-shorteners"

redirectTo :: Char.ByteString -> Response
redirectTo url = responseLBS status302 [("Location", url)] ""

shortenHandler :: Request -> IO Response
shortenHandler request = do
    let
        url = extractUrl request
        headers = []
    _ <- updateShortenIORef url
    token <- lastToken <$>
             readIORef world
    let
        body = encode $ "/" ++ token
    return $ responseLBS status201 headers body

extractUrl :: Request -> Url
extractUrl request = case url of
                         Just url' -> Char.unpack $ fromJust url'
                         Nothing -> ""
                     where url = lookup "url" $ queryString request

expandHandler :: Request -> IO Response
expandHandler request = do
        url <- expandTokenIORef $ extractToken request
        case url of
            Nothing -> return $ responseLBS status404 [] ""
            Just url' -> return $ redirectTo $ Char.pack url'

extractToken :: Request -> Token
extractToken request =
        case pathInfo request of
            [] -> Text.unpack ""
            [a] -> Text.unpack a
            _ -> ""

updateShortenIORef :: Url -> IO ()
updateShortenIORef url = modifyIORef world (shorten url)

expandTokenIORef :: Token -> IO (Maybe Url)
expandTokenIORef token = do
    currentWorld <- readIORef world
    return $ expand token currentWorld
