module ParsecSql
  ( findSelects
  , skipSpaces
  , skipSpaces1
  )
where

import ParsecUtils
import SqlSyntax
import Text.Parsec hiding ((<|>), many, optional, State, string)
import Control.Applicative
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Functor (void)
import Data.Maybe (fromMaybe)

string = stringCSI

-- | Пропускает все разделительные символы (пробельные символы и комментарии).
-- Всегда заканчивается удачно, даже если неичего подобного не нашлось.
-- Возвращает один пробел (если пробельные символы нашлись) или пустую строку если ничего не было найдено.
skipSpaces :: CharParser st [Char]
skipSpaces =
  skipSpaces1 <|> return ""

-- | Пропускает все разделительные символы (пробельные символы и комментарии).
-- Заканчивается удачно только если был найден хоть один разделительный символ.
-- Возвращает один пробел.
skipSpaces1 :: CharParser st [Char]
skipSpaces1 =
  try (skipMany1 (sqlComment <|> void space1')) >> return " "
  where
    space1' = space >> spaces >> return " "

sqlBlockComment :: CharParser st String
sqlBlockComment = do
  string start <?> "block comment start \"/*\""
  s1 <- manyTill anyChar (try $ lookAhead $ string end)
  string end <?> "block comment end \"*/\""
  return $ start ++ dropEndLineSpaces s1 ++ end
  where
    start = "/*"
    end   = "*/"

sqlEndLineComment :: CharParser st String
sqlEndLineComment = do
  string "--" <?> "line comment start \"--\""
  s1 <- manyTill anyChar (lookAhead $ (eol >> return "\n" ) <|> (eof >> return ""))
  s2 <- (eol >> return "\n") <|> (eof >> return "")
  return $ "--" ++ dropWhileEnd isSpace s1 ++ s2

sqlComment :: CharParser st ()
sqlComment =
  void $ sqlBlockComment <|> sqlEndLineComment

sqlSelect :: CharParser st SqlSelect
sqlSelect = do
  fields <- string "select" *> optional (try $ skipSpaces1 >> string "distinct") *> skipSpaces1 *> sqlSelectFields <* sqlFrom <* skipTillSqlExpressionEnd
  return $ SqlSelect { selectFields = fields }

sqlFrom :: CharParser st ()
sqlFrom =
  void $ string "from" *> void skipSpaces1

sqlSelectFields :: CharParser st [SqlSelectField]
sqlSelectFields =
   sqlSelectField `sepBy1` (char ',' *> skipSpaces)

sqlSelectField :: CharParser st SqlSelectField
sqlSelectField =
  SqlSelectField <$> sqlExpression <*> optionMaybe sqlFieldName
  where
    sqlFieldName =
       optional (string "as" *> skipSpaces) *> sqlSingleIdentifierWithExclusions ["from"] <* skipSpaces

sqlExpression :: CharParser st SqlExpression
sqlExpression =
  sqlExpression' []

sqlExpression'' :: [String] -> CharParser st SqlExpression
sqlExpression'' exclusions =
  (
   sqlParenthesis
   <|>
   sqlNumberLiteral
   <|>
   sqlStringLiteral
   <|>
   sqlVariable
   <|>
   try sqlCast
   <|>
   try sqlFunction
   <|>
   try sqlCase
   <|>
   SqlESelect <$> try sqlSelect
   <|>
   try (sqlUnaryOperator exclusions)
   <|>
   try sqlEAny
   <|>
   sqlEIdentifier exclusions
  ) <* skipSpaces

sqlExpression' :: [String] -> CharParser st SqlExpression
sqlExpression' exclusions =
   try (sqlBinaryOperator <* skipSpaces)
   <|>
   sqlExpression'' exclusions

sqlParenthesis :: CharParser st SqlExpression
sqlParenthesis =
  char '(' *> (SqlEParenthesis <$> sqlExpression) <* char ')' <* skipSpaces


sqlUnquotedIdentifierS :: [String] -> CharParser st String
sqlUnquotedIdentifierS exclusions = do
  mapM_ (\s -> notFollowedBy (string s >> skipSpaces)) exclusions
  (:) <$> sqlIdentifierStartChar <*> many sqlIdentifierChar

sqlSingleIdentifierWithExclusions :: [String] -> CharParser st SqlSingleIdentifier
sqlSingleIdentifierWithExclusions exclusions =
  (
    SqlSingleIdentifierQuoted <$> (char '"' *> many (noneOf "\"") <* char '"')
    <|>
    SqlSingleIdentifier <$> sqlUnquotedIdentifierS exclusions
  ) <* skipSpaces

sqlIdentifier :: CharParser st SqlIdentifier
sqlIdentifier =
  sqlIdentifierWithExclusions []

sqlIdentifierWithExclusions :: [String] -> CharParser st SqlIdentifier
sqlIdentifierWithExclusions exclusions = do
  ids <- sqlSingleIdentifierWithExclusions exclusions `sepBy1` (char '.' >> skipSpaces)
  skipSpaces
  return $ SqlIdentifier (init ids) (last ids)

sqlEIdentifier :: [String] -> CharParser st SqlExpression
sqlEIdentifier exclusions =
  SqlEIdentifier <$> sqlIdentifier

sqlFunction :: CharParser st SqlExpression
sqlFunction = do
  name <- sqlIdentifier
  optional spaces
  char '('
  skipSpaces
  args <- sqlExpression `sepBy` (char ',' *> skipSpaces)
  skipSpaces
  char ')'
  skipSpaces
  return $ SqlEFunction name args

sqlNumberLiteral :: CharParser st SqlExpression
sqlNumberLiteral = do
  d1 <- many1 digit
  d2 <- option "" (char '.' *> many digit)
  skipSpaces
  return $ SqlENumberLiteral $
    if null d2
      then d1
      else d1 <> "." <> d2

sqlStringLiteralS :: CharParser st String
sqlStringLiteralS = do
  string "'" <?> "string literal start \"'\""
  s <- try (string "''") <|> many (noneOf "'")
  string "'" <?> "string literal end \"'\""
  skipSpaces
  return s

sqlStringLiteral :: CharParser st SqlExpression
sqlStringLiteral =
  SqlEStringLiteral <$> sqlStringLiteralS


sqlCase :: CharParser st SqlExpression
sqlCase = do
  string "case" >> skipSpaces1
  base <- (notFollowedBy (string "when" >> skipSpaces1) >> optionMaybe sqlExpression) <|> return Nothing
  whens <- many1 sqlWhen
  else' <- optionMaybe $ try (string "else") *> skipSpaces *> sqlExpression
  string "end" <?> "case \"end\""
  skipSpaces
  return $ SqlECase base whens else'
  where
    sqlWhen = do
      string "when" <?> "case \"when\""
      skipSpaces
      condition <- dropWhileEnd isSpace <$> manyTill1 anyChar (try (string "then" *> skipSpaces))
      value <- sqlExpression
      return (condition, value)

sqlVariable :: CharParser st SqlExpression
sqlVariable =
  SqlEVariable <$> (char ':' *> sqlUnquotedIdentifierS [] <* skipSpaces)

sqlCast :: CharParser st SqlExpression
sqlCast = do
  string "cast" >> skipSpaces
  char '(' >> skipSpaces
  expr <- sqlExpression
  string "as" >> skipSpaces
  type' <- sqlType
  char ')' >> skipSpaces
  return $ SqlECast { castExpression = expr,  castAs = type' }

sqlType :: CharParser st SqlType
sqlType =
  sqlTypeVarchar2 <|> sqlTypeVarchar <|> sqlTypeNumber <|> sqlTypeAny

sqlSizedType :: String -> CharParser st (Int, Maybe String)
sqlSizedType typeName = do
  string typeName >> skipSpaces
  char '(' >> skipSpaces
  size <- read <$> many1 digit
  typeSpec <- optionMaybe $ skipSpaces1 *> sqlUnquotedIdentifierS []
  skipSpaces >> char ')' >> skipSpaces
  return (size, typeSpec)

sqlTypeVarchar2 :: CharParser st SqlType
sqlTypeVarchar2 = do
   (size, typeSpec) <- sqlSizedType "varchar2"
   return $ SqlTypeVarchar2 { varchar2Size = size, varchar2TypeSpecifier = typeSpec }

sqlTypeVarchar :: CharParser st SqlType
sqlTypeVarchar = do
   (size, typeSpec) <- sqlSizedType "varchar"
   return $ SqlTypeVarchar { varcharSize = size, varcharTypeSpecifier = typeSpec }

sqlTypeNumber :: CharParser st SqlType
sqlTypeNumber = do
  string "number" >> skipSpaces
  sizes <- optionMaybe $ do
    char '(' >> skipSpaces
    size1 <- read <$> many1 digit
    skipSpaces
    size2 <- (read <$>) <$> optionMaybe (char ',' >> skipSpaces >> many1 digit)
    skipSpaces >> char ')' >> skipSpaces
    return (size1, size2)
  return $ SqlTypeNumber sizes

sqlTypeAny :: CharParser st SqlType
sqlTypeAny =
  SqlTypeAny <$> many1 (noneOf "()")

skipTillSqlExpressionEnd :: CharParser st [Char]
skipTillSqlExpressionEnd = do
  (<>) . concat
    <$>
    many1 (
           notFollowedBy (selectEndMark <|> fmap (:[]) (char ')') <|> (string "select" >> skipSpaces1))
           >>
           skipSpaces1 <|> sqlStringLiteralS <|> parenthesisPair <|> ((:[]) <$> satisfy (/= ')'))
          )
    <*>
    (fromMaybe "" <$> optional selectEndMark)
  where
    selectEndMark =
      plus <|> minus <|> try unionAll <|> union <|> (eof >> return "")
    union = (<>) <$> string "union" <*> skipSpaces
    unionAll = (\a b c d -> a <> b <> c <> d) <$> string "union" <*> skipSpaces1 <*> string "all" <*> skipSpaces1
    plus = (<>) <$> string "plus" <*> skipSpaces1
    minus = (<>) <$> string "minus" <*> skipSpaces1

skipTillNextSqlExpression :: CharParser st [Char]
skipTillNextSqlExpression =
    skipSomething <|> return ""
    where
      skipSomething = do
        notFollowedBy (string "select" >> skipSpaces1)
        x <- fmap concat $ do
          many $ do
            notFollowedBy (skipSpaces1 >> string "select" >> skipSpaces1)
            skipSpaces1 <|> sqlStringLiteralS <|> fmap (:[]) anyChar
        y <- skipSpaces
        return $ x <> y

parenthesisPair :: CharParser st [Char]
parenthesisPair = do
  p1 <- (:) <$> char openP <*> skipSpaces
  p2 <- concat <$> many (parenthesisPair <|> fmap reLiterate sqlStringLiteralS <|> skipSpaces1 <|> ((:[]) <$> satisfy (/= closeP)))
  p3 <- (:) <$> char closeP <*> skipSpaces
  return $ p1 <> p2 <> p3
  where
    openP = '('
    closeP = ')'
    reLiterate s = "'" <> s <> "'"

sqlUnaryOperator :: [String] -> CharParser st SqlExpression
sqlUnaryOperator notTheseIdentifiers = do
  sign <- (:[]) <$> oneOf "-+"
  skipSpaces
  arg <- sqlExpression'' notTheseIdentifiers
  return $ SqlEUnaryOperator { unaryOperatorName = sign, unaryOperatorArgument = arg }

sqlBinaryOperator :: CharParser st SqlExpression
sqlBinaryOperator = do
  operand1 <- sqlExpression'' []
  skipSpaces
  operator <- (:[]) <$> oneOf "+-*/%" <|> string "||"
  skipSpaces
  operand2 <- sqlExpression
  return $ SqlEBinaryOperator
           { binaryOperatorOperand1 = operand1
           , binaryOperatorName = operator
           , binaryOperatorOperand2 = operand2
           }

sqlEAny :: CharParser st SqlExpression
sqlEAny =
  SqlEAny <$> many (sqlSingleIdentifierWithExclusions [] <* char '.' <* skipSpaces) <* char '*' <* skipSpaces

findSelects :: CharParser st [SqlSelect]
findSelects =
  skipSpaces *> many (sqlSelect <* skipTillNextSqlExpression)
