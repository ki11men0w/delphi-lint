{- -*- coding:utf-8 -*- -}

module Dfm where

import Text.Parsec hiding ((<|>), many, optional)
import Control.Monad
import Control.Applicative
import ParsecUtils
import Debug.Trace
import Data.Maybe (fromMaybe)
import Numeric (readHex)
import Data.Char (chr, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.String (IsString(..))

type DfmFile = Object

data Object = Object {
    objectName :: String
  , objectType :: String
  , objectSource :: ObjectSource
  , objectExtraOptions :: Maybe String
  , objectProperties :: [PropertyOfObject]
  } deriving (Show)

data ObjectSource = Normal | Inherited | Inline deriving (Show)


objectS = "object"
inheritedS = "inherited"
inlineS = "inline"
instance IsString ObjectSource where
  fromString inheritedS = Inherited
  fromString inlineS = Inline
  fromString _ = Normal

data PropertyOfObject = PropertyO Object | PropertyP Property
  deriving (Show)

data Property = Property
  {
    propertyName :: String
  , propertyValue :: PropertyValue
  }
  deriving (Show)

data PropertyValue =
      PVName String
    | PVBoolean Bool
    | PVInteger Integer
    | PVString String
    | PVList [String]
    | PVStrings [String]
    | PVBinary String
    | PVSet [String]
    | PVItems [Item]
  deriving (Show)


data Item = Item {itemProperties :: [PropertyOfObject]}
            deriving (Show)

type Parser = Parsec String ()

traceM' label = return ()
-- traceM' label = do
--   x <- ((take 100)) <$> getInput
--   traceM $ label ++ ' ':x
--   return ()

parseDfmFile :: String -> String -> Either String Object
parseDfmFile sourceName input =
  case parse parseObject sourceName input of
    Left e -> (Left . show) e
    Right x -> Right x

isTextDfm :: String -> Bool
isTextDfm s =
  or $ (`isPrefixOf` s) <$> [objectS, inheritedS, inlineS]

parseObject :: Parser Object
parseObject = do
  traceM' $ "parseObject [ "
  skipSpaces'
  objSource <- fromString <$> (string objectS <|> try (string inheritedS) <|> string inlineS)
  char ' '
  name <- many1 $ noneOf ":"
  string ": "
  type' <- many1 $ noneOf spaceChars
  extraOpts <- optionMaybe $ char ' ' *> restOfLine'
  eol
  properties <- parseProperties
  endOfObject
  traceM' "parseObject ]"
  return $ Object name type' objSource extraOpts properties


-- | Выбирает из буфера строку соотсетсвующую концу объекта.
endOfObject :: Parser ()
endOfObject = do
  traceM' "endOfObject ["
  skipSpaces' >> string "end" >> spacesTillEol
  traceM' "endOfObject ]"
  return ()

-- | Срабатывает если находимся в маркере конца объекта. Из буфера ничего не считывается.
endOfObject' :: Parser ()
endOfObject' = do
  traceM' "endOfObject' ["
  try $ lookAhead endOfObject
  traceM' "endOfObject' ]"
  return ()

endOfObject'' :: Parser ()
endOfObject'' = endOfObject' <|> endOfItem'


parseProperties :: Parser [PropertyOfObject]
parseProperties = do
  traceM' "parseProperties ["
  value <- manyTill parsePropertyOfObject endOfObject''
  traceM' "parseProperties ]"
  return value


parsePropertyOfObject :: Parser PropertyOfObject
parsePropertyOfObject = do
  traceM' "parsePropertyOfObject ["
  value <- PropertyO <$> (try parseObject) <|> parseProperty <?> "property of object"
  traceM' "parsePropertyOfObject ]"
  return value

parseProperty :: Parser PropertyOfObject
parseProperty = do
  traceM' "parseProperty ["
  skipSpaces'
  propName <- many1 $ noneOf " "
  string " = "
  propValue <- parsePropertyValue
  traceM' "parseProperty ]"
  return $ PropertyP $ Property propName propValue
      
parsePropertyValue = do
  traceM' "parsePropertyValue ["
  value <- between skipSpaces' spacesTillEol $
           parseSet <|> parseList <|> parseBinary <|> parseItems <|> try parseInteger <|> try parseBoolean <|> try parseString <|>  parseName
  traceM' "parsePropertyValue ]"
  return value
                                 
parseBoolean :: Parser PropertyValue
parseBoolean = do
  traceM' "parseBoolean ["
  x <- PVBoolean <$> (True <$ string "True" <|> False <$ string "False")
  traceM' "parseBoolean ]"
  return x

parseInteger :: Parser PropertyValue
parseInteger = do
  traceM' "parseInteger ["
  sign <- fromMaybe "" <$> (optional $ string "-")
  value <- many1 digit
  lookAhead eol
  traceM' "parseInteger ]"
  (return . PVInteger . read) $ sign ++ value

parseString :: Parser PropertyValue
parseString = do
  traceM' "parseString ["
  value <- PVString `fmap` parseString'
  traceM' "parseString ]"
  return value

parseString' :: Parser String
parseString' =
  optional spaces *> (concat <$> oneLinePart `sepBy1` separator)
    where
      oneLinePart :: Parser String
      oneLinePart = concat <$> (many1 oneLineChank)
      oneLineChank :: Parser String
      oneLineChank = normalString <|> symbolsString
      normalString :: Parser String
      normalString = between stringQM stringQM $ many (noneOf "'") <|> try (string "''")
      symbolsString :: Parser String
      symbolsString = many1 hexChar
      hexChar :: Parser Char
      hexChar =  chr . read <$> (char '#' *> many1 digit)
      separator :: Parser ()
      separator = string " +" >> eol >> skipSpaces' >> return ()

-- parseStrings :: Parser PropertyValue
-- parseStrings = do
--   traceM' "parseStrings ["
--   value <- PVStrings `fmap` (between (char '(' >> spaces) (char ')') (parseString' `sepBy` spaces1))
--   traceM' "parseStrings ]"
--   return value

parseListItem :: Parser String
parseListItem = do
  traceM' "parseListItem ["
  value <- many $ noneOf ")\n\r"
  traceM' "parseListItem ]"
  return value

parseList :: Parser PropertyValue
parseList = do
  traceM' "parseList ["
  value <- between (char '(' >> spaces) (char ')') $ PVStrings <$> strings' <|> PVList <$> items'
  traceM' "parseList ]"
  return value
    where
      strings' = do
              traceM' "strings' ["
              value <- parseString' `sepBy1` spaces1
              traceM' "strings' ]"
              return value
      items' = parseListItem `sepBy` spaces1

parseSet :: Parser PropertyValue
parseSet = do
  traceM' "parseSet ["
  value <- fmap PVSet $ between (char '[') (char ']') (many1 (noneOf ",]") `sepBy` string ", ")
  traceM' "parseSet ]"
  return value

parseBinary :: Parser PropertyValue
parseBinary = do
  traceM' "parseBinary ["
  value <- PVBinary <$> (between (char '{' >> optional (eol >> spaces)) (char '}') (concat <$> ((many1 hexDigit) `sepBy` spaces)))
  traceM' "parseBinary ]"
  return value

parseName :: Parser PropertyValue
parseName = do
  traceM' "parseName ["
  value <- fmap PVName $ notSpaces
  traceM' "parseName ]"
  return value

parseItems :: Parser PropertyValue
parseItems = do
  traceM' "parseItems ["
  value <- between (char '<' >> (optional eol)) (char '>') $ many parseItem
  traceM' "parseItems ]"
  return $ PVItems value

parseItem :: Parser Item
parseItem = do
  traceM' "parseItem ["
  value <- Item <$> (between startOfItem endOfItem parseProperties)
  traceM' "parseItem ]"
  return value
    where
      startOfItem = skipSpaces' >> string "item" >> eol

-- | Выбирает из буфера строку соотсетсвующую концу item.
endOfItem :: Parser ()
endOfItem = do
  traceM' "endOfItem ["
  skipSpaces' >> string "end" >> (eol <|> (lookAhead (char '>') >> return ()))
  traceM' "endOfItem ]"
  return ()

-- | Срабатывает если находимся в маркере конца item. Из буфера ничего не считывается.
endOfItem' :: Parser ()
endOfItem' = do
  traceM' "endOfItem' ["
  try $ lookAhead endOfItem
  traceM' "endOfItem' ]"
  return ()


stringQM = char '\''
  
  
  
