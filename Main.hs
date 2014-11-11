{-# LANGUAGE OverloadedStrings , NoImplicitPrelude #-}

import           Data.Binary (encode)
import qualified Data.ByteString.Char8 as CBS
import           Data.Global (declareIORef)
import           Data.IORef (IORef, readIORef, modifyIORef) 
import           Data.Maybe (Maybe (..) , fromJust)
import           Data.Text (unpack)
import Network.HTTP.Types (status302,
                           status201,
                           status404)
import           Network.Wai (Request, Response,Application, pathInfo, responseLBS, queryString, ResponseReceived)
import           Network.Wai.Handler.Warp (run)
import           Prelude (($), IO , putStrLn, (++) , lookup , (.) ) 
import           System.IO.Unsafe
import           Shortener

world :: IORef World
world = declareIORef "world" initialWorld

main :: IO ()
main = do
        putStrLn "listening on 8080"
        run 8080 app

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app request respond = do
    respond.responseBuilder $ request


responseBuilder request =  case pathInfo request of
  [] -> indexHandler
  ["shorten"] -> shortenHandler request
  _ -> expandHandler request


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
                     where url = lookup "url" $ queryString request

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
