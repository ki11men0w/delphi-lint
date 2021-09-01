{- -*- coding:utf-8 -*- -}

module Dfm (
            -- * DFM representation
            DfmFile
           ,Object(..)
           ,ObjectOrigin(..)
           ,PropertyOfObject(..)
           ,Property(..)
           ,PropertyValue(..)
           ,Item(..)
           -- * Parsing functions
           ,ParseDfmOpts(..)
           ,parseDfmFile
           ) where

import Text.Parsec hiding ((<|>), many, optional)
import Control.Applicative
import Control.Monad (void)
import ParsecUtils
import Debug.Trace
import Data.Maybe (fromMaybe)
import Data.Char (chr)
import Data.List (isPrefixOf)
import Data.String (IsString(..))


-- | DFM-file as a hole
type DfmFile = Object

-- | Object in DFM-file
data Object = Object {
    objectName :: String -- ^ Object name
  , objectType :: String -- ^ Object type
  , objectOrigin :: ObjectOrigin -- ^ How object declared: object, inherited or inline
  , objectExtraOptions :: Maybe String -- ^ Extra declarations that following declaration of object at the same line.
                                       -- I don't now what these declarations mean. For example @__[2]__@ in:
                                       --
                                       -- @
                                       --   object frmMain: TForm __[2]__
                                       --     Left = 20
                                       --     Top = 100
                                       --   ...
                                       -- @
                                       --
  , objectProperties :: [PropertyOfObject] -- ^ List of object properties
  } deriving (Show)

-- | Object origin type
data ObjectOrigin = Normal | Inherited | Inline deriving (Show)

objectS = "object"
inheritedS = "inherited"
inlineS = "inline"
instance IsString ObjectOrigin where
  fromString inheritedS = Inherited
  fromString inlineS = Inline
  fromString _ = Normal

-- | Represent any object property 
data PropertyOfObject
  = PropertyO Object -- ^ Propety that is itself object
  | PropertyP Property -- ^ Simple property of object
  deriving (Show)

-- | Single simple property
data Property = Property
  {
    propertyName :: String -- ^ Property name
  , propertyValue :: PropertyValue -- ^ Property value
  }
  deriving (Show)

-- | Represent value od a simple property
data PropertyValue =
      PVConstant String -- ^ Constant property
    | PVBoolean Bool -- ^ Boolean property
    | PVInteger Integer -- ^ Integer property
    | PVString String -- ^ String property
    | PVList [String] -- ^ Property with list
    | PVStrings [String] -- ^ TStrings property
    | PVBinary String -- ^ Binary property
    | PVSet [String] -- ^ Propertty with set of values
    | PVItems [Item] -- ^ Property with @items@
  deriving (Show)

-- | Representation of single @item@
newtype Item = Item {itemProperties :: [PropertyOfObject]}
            deriving (Show)


type Parser = Parsec String ()

traceM' label = return ()
-- traceM' label = do
--   x <- ((take 100)) <$> getInput
--   traceM $ label ++ ' ':x
--   return ()

-- | Options of DFM parsing
newtype ParseDfmOpts = ParseDfmOpts {
    ignoreBinaryDfm :: Bool -- ^ Ignore not text DFM-files
  }

-- | Parse DFM file. Now we can parse only DFM-files in Text format.
parseDfmFile :: FilePath -> ParseDfmOpts -> IO (Either String (Maybe Object))
parseDfmFile file opts = do
  input <- readFile file
  return $ if not $ isTextDfm input
    then
      if ignoreBinaryDfm opts
        then Right Nothing
        else Left "not text DFM"
    else
      case parse parseObject file input of
        Left e -> (Left . show) e
        Right x -> Right $ Just x

isTextDfm :: String -> Bool
isTextDfm s =
  or $ (`isPrefixOf` s) <$> [objectS, inheritedS, inlineS]

parseObject :: Parser Object
parseObject = do
  traceM' "parseObject [ "
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
  value <- PropertyO <$> try parseObject <|> parseProperty <?> "property of object"
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
           parseSet <|> parseList <|> parseBinary <|> parseItems <|> try parseInteger <|> try parseBoolean <|> try parseString <|> parseConstant
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
  sign <- fromMaybe "" <$> optional (string "-")
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
      oneLinePart = concat <$> many1 oneLineChank
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
  value <- PVSet <$> between (char '[') (char ']') (many1 (noneOf ",]") `sepBy` string ", ")
  traceM' "parseSet ]"
  return value

parseBinary :: Parser PropertyValue
parseBinary = do
  traceM' "parseBinary ["
  value <- PVBinary <$> between (char '{' >> optional (eol >> spaces)) (char '}') (concat <$> (many1 hexDigit `sepBy` spaces))
  traceM' "parseBinary ]"
  return value

parseConstant :: Parser PropertyValue
parseConstant = do
  traceM' "parseConstant ["
  value <- PVConstant <$> notSpaces
  traceM' "parseConstant ]"
  return value

parseItems :: Parser PropertyValue
parseItems = do
  traceM' "parseItems ["
  value <- between (char '<' >> optional eol) (char '>') $ many parseItem
  traceM' "parseItems ]"
  return $ PVItems value

parseItem :: Parser Item
parseItem = do
  traceM' "parseItem ["
  value <- Item <$> between startOfItem endOfItem parseProperties
  traceM' "parseItem ]"
  return value
    where
      startOfItem = skipSpaces' >> string "item" >> eol

-- | Выбирает из буфера строку соотсетсвующую концу item.
endOfItem :: Parser ()
endOfItem = do
  traceM' "endOfItem ["
  skipSpaces' >> string "end" >> (eol <|> void (lookAhead (char '>')))
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
  
  
  
