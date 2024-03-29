{- -*- coding:utf-8 -*- -}

module Checks.DfmLocalizationProblemsInSql
  (
    DfmCheckConfig(..)
  , checkDfm
  ) where

import Dfm
import ParsecUtils
import ParsecSql
import SqlSyntax
import Control.Monad.Trans.State (State, execState, get, put)
import Data.List (intercalate, uncons)
import Text.Parsec (parse, string)
import Control.Applicative
import Data.Char (toLower, toUpper)
import Data.Functor (void)
import Data.Either (isRight)

data DfmCheckConfig
  = DfmCheckConfig
    { ignoreUnparseableSql :: Bool
    , ignoreNumericFieldsAmbiguousSize :: Bool
    , includeSqlInReport :: Bool
    }

newtype StateData = StateData
  {
    messages :: [String]
  } deriving (Show)

type ProblemReport = String


-- | Check DFM for non ASCII symbols in probably SQL's
checkDfm :: DfmCheckConfig -> DfmFile -> [ProblemReport]
checkDfm cfg dfm =
  messages $ execState (checkObject cfg dfm []) $ StateData []

-- | Проверяет объект 'o' и все его дочерние объекты.
-- Параметр 'parents' содержит ссылки на все родительские объекты
-- объекта 'o'.
checkObject :: DfmCheckConfig -> Object -> [Object] -> State StateData ()
checkObject cfg o parents = do
  mapM_ checkProperty $ objectProperties o
   where
     checkProperty (PropertyO child) = checkObject cfg child (o:parents)
     checkProperty (PropertyP p@(Property _ (PVString s))) = checkProp cfg (makePropName'''' p o parents) s
     checkProperty (PropertyP p@(Property _ (PVStrings ss))) = checkProp cfg (makePropName'''' p o parents) $ intercalate "\n" ss
     checkProperty _ = return ()

checkProp :: DfmCheckConfig -> String -> String -> State StateData ()
checkProp cfg name s = do
  case checkSqlWithNonAsciiSymbols s of
    Nothing -> return ()
    Just msg -> do
           st <- get
           put $ st {messages = makeMessage name msg : messages st}
  case checkSqlWithAmbiguousStringFiledSize cfg s of
    Nothing -> return ()
    Just msg -> do
           st <- get
           put $ st {messages = makeMessage name msg : messages st}


makeMessage :: String -> String -> String
makeMessage propName errMsg =
  "Property \"" ++ propName ++ "\": \"" ++ errMsg ++ "\""

makePropName :: [String] -> String
makePropName names = intercalate "." $ reverse names

makePropName'''' :: Property -> Object -> [Object] -> String
makePropName'''' p o os =
  makePropName $ propertyName p : objectName o : (objectName <$> os)

checkSqlWithNonAsciiSymbols :: String -> Maybe String
checkSqlWithNonAsciiSymbols s =
  if hasNotAscii s && hasSql s then Just $ "Non ASCII symbols in probably SQL expression: \'" ++ filterNonAscii s ++ "'"  else Nothing
    where
      filterNonAscii = filter (not .isAscii)
      hasNotAscii = not . all isAscii

hasSql :: String -> Bool
hasSql sql =
  isRight $ parse parseHasSql "may be sql" (toLower <$> sql)
  where
    parseHasSql :: CharParser st ()
    parseHasSql =
      void $ (skipSpaces1 >> sqlMarker <|> sqlMarker) >> skipSpaces1
      where
        sqlMarker =
              string "select"
          <|> string "where"
          <|> string "from"
          <|> string "order" >> skipSpaces1 >> string "by"
          <|> string "update"
          <|> string "insert"
          <|> string "begin"

hasSelect :: String -> Bool
hasSelect sql =
    isRight $ parse parseHasSelect "may be select" (toLower <$> sql)
    where
      parseHasSelect :: CharParser st ()
      parseHasSelect =
          void $ ((skipSpaces1 >> string "select") <|> string "select") >> skipSpaces1

checkSqlWithAmbiguousStringFiledSize :: DfmCheckConfig -> String -> Maybe String
checkSqlWithAmbiguousStringFiledSize cfg sql =
  if hasSelect sql
    then
      case parse findSelects "probably SQL expression with SELECT" sql of
        Right selects -> findInSelects selects
        Left err -> if ignoreUnparseableSql cfg
                      then Nothing
                      else Just $ "Cannot parse probably SQL expression: " <> show err <> "\n" <> sql
    else Nothing
  where
    findInSelects :: [SqlSelect] -> Maybe String
    findInSelects selects =
      let errs = concat $ findInSelect <$> selects
      in if null errs
           then Nothing
           else Just $ "Maybe ambiguous field size because of: " <> intercalate ", " (toSql <$> errs)
                        <> (if includeSqlInReport cfg then "\n```\n" <> sql <> "\n```" else "")

    findInSelect :: SqlSelect -> [SqlExpression]
    findInSelect (SqlSelect fields) =
      concat $ [findInExpr e | e <- fieldValue <$> fields, isNotDate e]
      where
        checkIfAll es =
          let errs = findInExpr <$> es
          in
            if any null errs
            then []
            else concat errs
        findInExpr :: SqlExpression -> [SqlExpression]
        findInExpr (SqlEFunction fn args) =
          let
            checkAllArgs = concat (findInExpr <$> args)
            checkOnlyFirstArgument = maybe [] findInExpr (fst <$> uncons args)
          in case simpleIdentifierName fn of
            Just "NVL" -> checkIfAll args
            Just "COUNT" -> []
            Just "TO_DATE" -> []
            Just "TO_NUMBER" -> checkIfAll args
            Just "TO_CHAR" -> concat (findInExpr <$> args)
            Just "DECODE" ->
              -- Берем только аргументы влияющие на тип результата этой функции
              let nextPair (_:e:es) = findInExpr e <> nextPair es
                  nextPair [y] = findInExpr y
                  nextPair [] = []
              in nextPair (tail args)
            Just "ROUND" ->
              case args of
                [a, _] -> findInExpr a
                _ -> []
            Just "ADD_MONTHS" -> checkOnlyFirstArgument
            Just "TRUNC" ->
              case args of
                -- для вызовов типа `trunc(sysdate, 'MM') проверяем только первый аргумент
                [e, SqlEStringLiteral _] -> findInExpr e
                _ -> checkAllArgs
            _ -> checkAllArgs
        findInExpr x@SqlENumberLiteral {} = [x  | not $ ignoreNumericFieldsAmbiguousSize cfg]
        findInExpr x@SqlEStringLiteral {} = [x]
        findInExpr x@SqlEVariable {} = [x]
        findInExpr (SqlEParenthesis v) = findInExpr v
        findInExpr (SqlESelect v) = findInSelect v
        findInExpr (SqlECase _ wn Nothing) = checkIfAll (snd <$> wn)
        findInExpr (SqlECase _ wn (Just el)) = checkIfAll (el:(snd <$> wn))
        findInExpr (SqlEUnaryOperator _ v) = findInExpr v
        findInExpr (SqlEBinaryOperator o1 _ o2) = checkIfAll [o1,o2]
        findInExpr _ = []

        isNotDate = not . isDate
        isDate :: SqlExpression -> Bool
        isDate (SqlEFunction fn args) =
          case simpleIdentifierName fn of
            Just "SYSDATE" -> True
            Just "TO_DATE" -> True
            Just "ADD_MONTHS" -> True
            Just "TRUNC" ->
              case args of
                [_, SqlEStringLiteral s] ->
                  (toUpper <$> s) `elem` ["Y", "MM", "D", "HH", "MI", "YYYY"]
                _ -> False
            _ -> any isDate args
        isDate (SqlEIdentifier i)
          | simpleIdentifierName i == Just "SYSDATE" = True
          | otherwise = False
        isDate (SqlECase _ wn Nothing) = any isDate (snd <$> wn)
        isDate (SqlECase _ wn (Just el)) = any isDate (el:(snd <$> wn))
        isDate _ = False
