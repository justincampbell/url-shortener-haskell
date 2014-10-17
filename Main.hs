{-# LANGUAGE OverloadedStrings #-}

import Snap

data Shortener = Shortener { _db :: String }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing shortenerInit
  quickHttpServe site

shortenerInit :: SnapletInit Shortener Shortener
shortenerInit = makeSnaplet "shortener" "DESC" Nothing $ do
  addRoutes [("/", indexHandler)]
  return Shortener { _db = "foo" }

indexHandler :: Handler Shortener Shortener ()
indexHandler = writeText "foo"
