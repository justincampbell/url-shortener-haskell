{-# LANGUAGE OverloadedStrings , NoImplicitPrelude #-}


import           Network.Wai.Handler.Warp (run)
import           Prelude (($), IO , putStrLn, (++) , lookup , (.) ) 
import           Shortener (app) 


main :: IO ()
main = do
        putStrLn "listening on 8080"
        run 8080 app


