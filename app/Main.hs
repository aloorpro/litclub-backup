module Main where

import GetFromXML
import System.Environment
import System.IO
import Network.HTTP
import Codec.Binary.UTF8.String

getPubs :: String -> IO String
getPubs username = do
    x <- simpleHTTP (getRequest $ "http://litclub.net/~"
                     ++ username ++ "/pubs/list")
    decodeString <$> getResponseBody x

main :: IO ()
main = do
    [f] <- getArgs
    x <- simpleHTTP (getRequest f)
    s <- decodeString <$> getResponseBody x
    putStrLn $ getTitle s ++ "\n\n" ++ getText s

