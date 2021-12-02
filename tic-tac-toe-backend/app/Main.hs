{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Web.Scotty.Trans as S
import Data.Text.Lazy (Text)

main :: IO ()
main = S.scottyT 5000 id routes


routes :: S.ScottyT Text IO ()
routes = do
    S.get "/1/" $ do
        S.text "Hello World!"