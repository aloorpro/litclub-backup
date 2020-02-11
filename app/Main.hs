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

toFile :: FilePath -> String -> IO ()
toFile path s = do
    raw <- getPub $ "http://litclub.net/libro/read.text-" ++ s ++ ".xml"
    writeFile (path ++ getTitle raw ++ ".txt") $ getText raw

pubs s = manyPubs <$> (getPubList s) 

main :: IO ()
main = do
    [name, path] <- getArgs
    ids <- pubs name
    toFile path $ head ids
    --let
    --putStrLn . show . length $ pubs
    --map (toFile path) pubs
    

