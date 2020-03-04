module GetFromXML
    ( getTitle
    , getText
    ) where

import Text.XML.Light
import Text.XML.Light.Cursor (getTag, Tag(..))
import Control.Applicative (Alternative(..))

-- tags for the cursor, this should match
titleTag = Tag {tagName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, 
    tagAttribs = [
        Attr {attrKey = QName {qName = "mr", qURI = Nothing, qPrefix = Just "xmlns"}
            , attrVal = "http://www.mirari.ru"},
        Attr {attrKey = QName {qName = "php", qURI = Nothing, qPrefix = Just "xmlns"}
            , attrVal = "http://php.net/xsl"},
        Attr {attrKey = QName {qName = "class", qURI = Nothing, qPrefix = Nothing}
            , attrVal = "mr-text-title"}
        ], tagLine = Just 1}
textTag = Tag {tagName = QName {qName = "tr", qURI = Nothing, qPrefix = Nothing}, 
    tagAttribs = [
        Attr {attrKey = QName {qName = "mr", qURI = Nothing, qPrefix = Just "xmlns"}
            , attrVal = "http://www.mirari.ru"},
        Attr {attrKey = QName {qName = "php", qURI = Nothing, qPrefix = Just "xmlns"}
            , attrVal = "http://php.net/xsl"},
        Attr {attrKey = QName {qName = "class", qURI = Nothing, qPrefix = Nothing}
            , attrVal = "mr-text-content"}
        ], tagLine = Just 1}

instance Eq Tag where
    a == b = (tagName a == tagName b) 
             && (tagAttribs a == tagAttribs b)

-- finding Element
findByTag :: Tag -> [Content] -> Maybe Element
findByTag _ [] = Nothing
findByTag t ((Elem x):xs) = ifTag t x 
            <|> findByTag t (elContent x)
            <|> findByTag t xs
findByTag t (_:es) = findByTag t es

ifTag :: Tag -> Element -> Maybe Element
ifTag t c | getTag c == t = Just c
          | otherwise = Nothing

-- collecting text
textFrom :: Tag -> [Content] -> [String]
textFrom t c = case findByTag t c of
    Just a -> allText $ elContent a
    Nothing -> []

allText :: [Content] -> [String]
allText [] = []
allText ((Elem x):xs) = allText (elContent x) ++ allText xs
allText ((Text x):xs) = cdData x : allText xs
allText (_:xs) = allText xs

-- exporting functions
getTitle :: String -> String
getTitle = concat . textFrom titleTag . parseXML
getText :: String -> String
getText  = unlines . textFrom textTag . parseXML