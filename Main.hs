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


main :: IO ()
main = do
        putStrLn "listening on 8080"
        run 8080 app


