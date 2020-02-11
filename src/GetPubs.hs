module GetPubs (
    parsePub, manyPubs
    ) where

import Text.Parsec
import Text.Parsec.Char
import Data.Either (rights)

parsePub :: Parsec String u [Char]
parsePub = do
    manyTill anyChar (try $ string "\"/libro/read.text-")
    many1 digit

manyPubs :: String -> [String]
manyPubs = rights . map (parse parsePub "") . lines 
    