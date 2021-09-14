{- -*- coding:utf-8 -*- -}
{-# LANGUAGE FlexibleContexts #-}

module ParsecUtils
  (
   CharParser
  ,anyLine
  ,eol
  ,manyTill1
  ,restOfLine
  ,restOfLine'
  ,searchFor
  ,searchForLine
  ,skipRestOfLine
  ,spaces1
  ,spacesTillEol
  ,spacesTillEol'
  ,stringCSI
  ,spaceChars
  ,skipSpaces'
  ,notSpace
  ,notSpaces
  ,isAscii
  ,ascii
  ,isSqlIdentifierChar
  ,sqlIdentifierChar
  ,isSqlIdentifierStartChar
  ,sqlIdentifierStartChar
  ,dropEndLineSpaces
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.Char (toUpper, toLower, isSpace, isAlpha, isDigit)
import Control.Monad (void)
import Data.Monoid
import Data.List (dropWhileEnd)


type CharParser st = GenParser Char st

spaceChars :: [Char]
spaceChars = "\n\r \t"

isAscii :: Char -> Bool
isAscii c = c `elem` " 0123456789qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM\n\r.,-_=+~`!@\"#$%^&*(){}[]<>;:'\\/|?\t"

ascii :: CharParser st Char
ascii = satisfy isAscii

isSqlIdentifierStartChar :: Char -> Bool
isSqlIdentifierStartChar c = isAlpha c || c `elem` "#$@_"

sqlIdentifierStartChar :: CharParser st Char
sqlIdentifierStartChar = satisfy isSqlIdentifierStartChar

isSqlIdentifierChar :: Char -> Bool
isSqlIdentifierChar c = isSqlIdentifierStartChar c || isDigit c

sqlIdentifierChar :: CharParser st Char
sqlIdentifierChar = satisfy isSqlIdentifierChar

-- | Ищет перевод строки соответствующий любому из сочетаний \"\n\", \"\r\", \"\n\r\", \"\r\n\"
eol :: CharParser st ()
eol =
  void
  ( try (string "\n\r")
    <|>
    try (string "\r\n")
    <|>
    string "\n"
    <|>
    string "\r"
    <?> "end of line" )

-- | Соответсвует любым символам до конца строки + конец строки. Возвращает все эти символы без
-- символов конца строки.
restOfLine :: CharParser st String
restOfLine = manyTill anyChar eol

-- | Соответсвует любым символам до конца строки + конец строки. Возвращает все эти символы без
-- символов конца строки. Символ конца строки остаётся в буфере.
restOfLine' :: CharParser st String
restOfLine' = manyTill anyChar (lookAhead (eof <|> eol))


anyLine = restOfLine

-- | Ищет следующее вхождение комбинатора переданного в кчестве аргумента. Возвращает то что вернул
-- этот комбинатор.
searchFor this =
  manyTill anyChar (try $ lookAhead this) >> this

-- | Ищет следующее вхождение комбинатора переданного в кчестве аргумента но только в тех случаях
-- когда он начинается с новой строки. Возвращает то что вернул этот комбинатор.
searchForLine this =
  manyTill restOfLine (try $ lookAhead this) >> this

-- | Пропускает остаток строки.
skipRestOfLine = void restOfLine

-- | Пропускает все конечные пробельные символы до конца строки. Сам символ конца строки удаляется из буфера.
spacesTillEol :: CharParser st ()
spacesTillEol = void $ manyTill space eol

-- | Пропускает все конечные пробельные символы до конца строки. Сам символ конца строки остаётся в
-- буфере.
spacesTillEol' :: CharParser st ()
spacesTillEol' = void $ manyTill space (lookAhead (eof <|> eol))

-- | Соответствует одному или большему кол-ву. пробельных символов.
spaces1 :: CharParser st ()
spaces1 = space >> spaces

-- | Пропускает все пробелы. Только пробелы: другие пробельные символы не учитываются.
skipSpaces' :: CharParser st ()
skipSpaces' = void $ many (char ' ')

-- | Case and count of spaces insensitive variant of 'string'
stringCSI :: String -> CharParser st String
stringCSI pattern_ =
  try $ foldl glue (return "") $ map test (unwords $ words pattern_)
  where test x = if isSpace x
                 then space >> spaces >> return " "
                 else (string [toUpper x] <|> string [toLower x])
                        <?> ("char '" <> [x] <> "' in string \"" <> pattern_ <> "\"")
        glue x y = do
          s1 <- x
          s2 <- y
          return $ s1 <> s2

manyTill1 searchFor end = do
  f <- notFollowedBy end >> searchFor
  o <- manyTill searchFor end
  return $ f:o

notSpace :: CharParser st Char
notSpace = satisfy (not . isSpace)

notSpaces :: CharParser st [Char]
notSpaces = many notSpace

dropEndLineSpaces :: String -> String
dropEndLineSpaces [] = []
dropEndLineSpaces s =
  let
    endEol = last s == '\n'
    ls = lines s
    lastLine = last ls
    initLines' = unlines $ map (dropWhileEnd isSpace) $ init ls
    lastLine' = if endEol then dropWhileEnd isSpace lastLine ++ "\n" else lastLine
  in
    initLines' ++ lastLine'
