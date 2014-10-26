{-# LANGUAGE OverloadedStrings #-}

import Data.Binary
import qualified Data.ByteString.Char8 as Char
import Data.Global
import Data.IORef
import Data.Maybe
import Data.Text as Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.IO.Unsafe

import Shortener

world :: IORef World
world = declareIORef "world" initialWorld

main :: IO ()
main = do
        putStrLn "listening on 8080"
        run 8080 app

app :: Application
app request respond =
    respond $ case pathInfo request of
        [] -> indexHandler
        ["shorten"] -> shortenHandler request
        _ -> expandHandler request

indexHandler :: Response
indexHandler = redirectTo "https://github.com/justincampbell/url-shorteners"

redirectTo :: Char.ByteString -> Response
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
                         Just url' -> Char.unpack $ fromJust url'
                         Nothing -> ""
                     where url = lookup "url" $ queryString request

expandHandler :: Request -> Response
expandHandler request =
        case url of
            Nothing -> responseLBS status404 [] ""
            Just url' -> redirectTo $ Char.pack url'
        where url = unsafeExpand $ extractToken request

extractToken :: Request -> Token
extractToken request =
        case pathInfo request of
            [] -> Text.unpack ""
            [a] -> Text.unpack a
            _ -> ""

unsafeShorten :: Url -> ()
unsafeShorten url = unsafePerformIO $ modifyIORef world (shorten url)

unsafeExpand :: Token -> Maybe Url
unsafeExpand token = expand token currentWorld where
    currentWorld = unsafePerformIO $ readIORef world
