module Main where

import GetFromXML
import GetPubs
import System.Environment
import System.IO
import Network.HTTP
import Codec.Binary.UTF8.String
import Data.List (intercalate)

getPub :: String -> IO String
getPub url = do
    x <- simpleHTTP (getRequest url)
    decodeString <$> getResponseBody x

getPubList :: String -> IO String
getPubList username = getPub $ "http://litclub.net/~"
                     ++ username ++ "/pubs/list"

main :: IO ()
main = do
    [f] <- getArgs
    s <- getPubList f
    let pubs = manyPubs s
    putStrLn $ show (length pubs) ++ "\n" ++ intercalate ", " pubs 

