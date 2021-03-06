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
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.Char (toUpper, toLower, isSpace)
import Control.Monad (void)
import Data.Monoid
import Data.Char (isAlphaNum)


type CharParser st = GenParser Char st

spaceChars :: [Char]
spaceChars = "\n\r \t"

isAscii :: Char -> Bool
isAscii c = c `elem` " 0123456789qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM\n\r.,-_=+~`!@\"#$%^&*(){}[]<>;:'\\/|?\t"

ascii :: CharParser st Char
ascii = satisfy isAscii

isSqlIdentifierChar :: Char -> Bool
isSqlIdentifierChar c = isAlphaNum c || c `elem` "#$@_"

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
skipSpaces' = many (char ' ') >> return ()

-- | Case and count of spaces insensitive variant of 'string'
stringCSI :: String -> CharParser st String
stringCSI pattern =
  try $ foldl glue (return "") $ map test (unwords $ words pattern)
  where test x = if isSpace x
                 then space >> spaces >> return " "
                 else string [toUpper x] <|> string [toLower x]
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
