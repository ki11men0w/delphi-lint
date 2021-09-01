{- -*- coding:utf-8 -*- -}

module Checks.DfmNonAsciiInSql
  (
   checkDfmForNonAsciiSymbolsInSql
  ) where

import Dfm
import Control.Monad.Trans.State (State, execState, get, put)
import Data.List (intercalate)
import ParsecUtils
import Text.Parsec hiding ((<|>), many, optional, State)
import Control.Applicative
import Data.Char (toLower)
import Data.Functor (($>))

newtype StateData = StateData
  {
    messages :: [String]
  } deriving (Show)


-- | Check DFM for non ASCII symbols in probably SQL's
checkDfmForNonAsciiSymbolsInSql :: DfmFile -> Maybe String
checkDfmForNonAsciiSymbolsInSql dfm =
  let msgs = messages $ execState (checkObject dfm []) $ StateData []
  in if null msgs
       then Nothing
       else Just $ intercalate "\n" msgs

-- | Проверяет объект 'o' и все его дочерние объекты.
-- Параметр 'parents' содержит ссылки на все родительские объекты
-- объекта 'o'.
checkObject :: Object -> [Object] -> State StateData ()
checkObject o parents = do
  mapM_ checkProperty $ objectProperties o
   where
     checkProperty (PropertyO child) = checkObject child (o:parents)
     checkProperty (PropertyP p@(Property _ (PVString s))) = checkProp (makePropName'''' p o parents) s
     checkProperty (PropertyP p@(Property _ (PVStrings ss))) = checkProp (makePropName'''' p o parents) $ intercalate "\n" ss
     checkProperty _ = return ()

checkProp name s =
  let m = checkSql s
  in case m of
    Nothing -> return ()
    Just msg -> do
           st <- get
           put $ st {messages = makeMessage name msg : messages st}
           


makeMessage propName errMsg =
  "Property \"" ++ propName ++ "\": \"" ++ errMsg ++ "\""

makePropName :: [String] -> String
makePropName names = intercalate "." $ reverse names

makePropName' :: String -> [String] -> String
makePropName' name names =
  let names' = name : names
  in intercalate "." $ reverse names'

makePropName'' :: [Object] -> String
makePropName'' objects =
  let names = map objectName objects
  in makePropName names

makePropName''' :: Object -> [Object] -> String
makePropName''' object objects =
  makePropName' (objectName object) (map objectName objects)

makePropName'''' :: Property -> Object -> [Object] -> String
makePropName'''' p o os =
  makePropName $ propertyName p : objectName o : map objectName os

checkSql :: String -> Maybe String
checkSql s =
  if hasNotAscii s && hasSql s then Just $ "Non ASCII symbols in probably SQL expression: \'" ++ filterNonAscii s ++ "'"  else Nothing
    where
      filterNonAscii = filter (not .isAscii)
      hasNotAscii = not . all isAscii
      hasSql :: String -> Bool
      hasSql s =
        let parseResult = parse parseHasSql "string property value" $ map toLower s
        in case parseResult of
             Left e -> False
             Right x -> True

parseHasSql :: Parsec String () ()
parseHasSql =
  () <$ try sqlMarkerAtBegin <|> () <$ manyTill anyChar sqlMarkerInbetween
  where
    notSqlIdentifierChar = satisfy (not . isSqlIdentifierChar)
    sqlMarker = string "select" <|> string "where" <|> string "from" <|> ((\_ _ -> "") <$> string "order" <* spaces1 <*> string "by")
    sqlMarkerAtBegin = sqlMarker *> notSqlIdentifierChar $> ()
    sqlMarkerInbetween = between notSqlIdentifierChar notSqlIdentifierChar sqlMarker
