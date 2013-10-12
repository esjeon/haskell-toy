
--
-- This is a toy JSON parser. I made this to test parsec.
--

module ToyJSON where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue =
      JSONBool Bool
    | JSONString String
    | JSONArray [JSONValue]
    | JSONObject [(String, JSONValue)]
    | JSONNumber Double
    deriving (Show)

ws :: Parser String
ws = many (oneOf " \t\n\r")

-- wrapped in whitespace
wsw :: Parser a -> Parser a
wsw p = ws *> p <* ws

litBool :: Parser Bool
litBool = wsw $
         (string "true"  *> pure True )
     <|> (string "false" *> pure False)

--litNumber :: Parser Float
--litNumber = do
--    wsw $ 

litString :: Parser String
litString = wsw $
    (char '"') *> (many $ noneOf ['\\', '"']) <* (char '"')
-- TODO: escape

litArray :: Parser [JSONValue]
litArray =
    (wsw $ char '[') *>
    ( jsonValue `sepBy` (wsw $ char ',') )
    <* (wsw $ char ']')

objectEntry :: Parser (String, JSONValue)
objectEntry = do
    key <- litString
    wsw $ char ':'
    value <- jsonValue
    return (key, value)

litObject :: Parser [(String, JSONValue)]
litObject =
    (wsw $ char '{') *>
    ( objectEntry `sepBy` (wsw $ char ',') )
    <* (wsw $ char '}')


jsonValue :: Parser JSONValue
jsonValue = (JSONBool <$> litBool)
        <|> (JSONString <$> litString)
        <|> (JSONArray <$> litArray)
        <|> (JSONObject <$> litObject)

jsonFile :: Parser JSONValue
jsonFile = jsonValue <* eof

