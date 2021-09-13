module SqlSyntax
  ( ToSql(..)
  , SqlType(..)
  , SqlSelect(..)
  , SqlSelectField(..)
  , SqlSingleIdentifier(..)
  , SqlIdentifier(..)
  , SqlExpression(..)
  , simpleIdentifierName
  )
where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Char (toUpper)

-- | Представляет описание типа. Например: `number`, `varchar2(100)`
data SqlType
  = SqlTypeVarchar { varcharSize :: Int, varcharTypeSpecifier :: Maybe String }
  | SqlTypeVarchar2 { varchar2Size :: Int, varchar2TypeSpecifier :: Maybe String }
  | SqlTypeNumber { numberLength :: Maybe (Int, Maybe Int) }
  | SqlTypeAny String  -- ^ Предсавляет любые другие возможные задания типа
  deriving (Show)

newtype SqlSelect
  = SqlSelect { selectFields :: [SqlSelectField] }
  deriving (Show)

-- | Отдельное поле select
data SqlSelectField
  = SqlSelectField
    { fieldValue :: SqlExpression             -- ^ Выражение содержащее значение поля
    , fieldName :: Maybe SqlSingleIdentifier  -- ^ Возможное название поля
    }
  deriving (Show)

-- | Отдельный (без уточнения схемы/пакета и т.п.) идентификатор
data SqlSingleIdentifier
  = SqlSingleIdentifier String           -- ^ Простой идентификатор, например: `call_id`
  | SqlSingleIdentifierQuoted String     -- ^ Идентификатор с явным указанием регистра символов
                                         -- с помощью взятия в кавычки. Наприер: `"CALLS"`
  deriving (Show)

-- | Представляет идентификатора с уточнением.
-- Например: `schenaname.packetname.function`
data SqlIdentifier
  = SqlIdentifier
      [SqlSingleIdentifier]        -- ^ "Пространстово имён"
      SqlSingleIdentifier          -- ^ Сам конечный идентификатор
  deriving (Show)

data SqlExpression
  = SqlEStringLiteral String
  | SqlENumberLiteral String
  | SqlEIdentifier SqlIdentifier
  | SqlEVariable String
  | SqlEParenthesis                            -- ^ Круглые скобки
      SqlExpression                            -- ^ Выражение внутри скобок
  | SqlEFunction                               -- ^ Функция
    { functionName :: SqlIdentifier            -- ^ Имя функции
    , functionArguments :: [SqlExpression]     -- ^ Аргументы фуккции
    }
  | SqlESelect SqlSelect
  | SqlECase                                    -- ^ Выражение `case`
    { caseExpression :: Maybe SqlExpression    -- ^ Возможное значение для выбора
    , caseWhens :: [
       ( String                                -- ^ условие
       , SqlExpression                         -- ^ результат
       )
      ]                                        -- ^ Список условий `when`
    , caseElse :: Maybe SqlExpression          -- ^ Условие `else`
    }
  | SqlECast
    { castExpression :: SqlExpression
    , castAs :: SqlType
    }
  | SqlEUnaryOperator
    { unaryOperatorName :: String
    , unaryOperatorArgument :: SqlExpression
    }
  | SqlEBinaryOperator
    { binaryOperatorOperand1 :: SqlExpression
    , binaryOperatorName :: String
    , binaryOperatorOperand2 :: SqlExpression
    }
  | SqlEAny [SqlSingleIdentifier]              -- ^ Соотвествует звёздочке: `*`, например: `select d.* from dual d`
  deriving (Show)



class ToSql a where
  toSql :: a -> String

instance ToSql SqlType where
  toSql (SqlTypeVarchar s t)  = "VARCHAR(" <> show s <> fromMaybe "" t <> ")"
  toSql (SqlTypeVarchar2 s t) = "VARCHAR2(" <> show s <> fromMaybe "" t <> ")"
  toSql (SqlTypeNumber Nothing) = "NUMBER"
  toSql (SqlTypeNumber (Just (s, Nothing))) = "NUMBER(" <> show s <> ")"
  toSql (SqlTypeNumber (Just (s, Just f))) = "NUMBER(" <> show s <> "," <> show f <> ")"
  toSql (SqlTypeAny t) = t

instance ToSql SqlSelect where
  toSql s = "select " <> intercalate ", " (toSql <$> selectFields s) <> " from ..."

instance ToSql SqlSelectField where
  toSql (SqlSelectField v Nothing) = toSql v
  toSql (SqlSelectField v (Just f)) = toSql v <> " as " <> toSql f

instance ToSql SqlSingleIdentifier where
  toSql (SqlSingleIdentifier s) = s
  toSql (SqlSingleIdentifierQuoted s) = "\"" <> s <> "\""

instance ToSql SqlIdentifier where
  toSql (SqlIdentifier [] s) = toSql s
  toSql (SqlIdentifier p s) = intercalate "." (toSql <$> p) <> "."  <> toSql s

instance ToSql SqlExpression where
  toSql (SqlEStringLiteral s) = "'" <> s <> "'"
  toSql (SqlENumberLiteral s) = s
  toSql (SqlEIdentifier i) = toSql i
  toSql (SqlEVariable s) = ":" <> s
  toSql (SqlEParenthesis e) = "(" <> toSql e <> ")"
  toSql (SqlEFunction n as) = toSql n <> "(" <> intercalate ", " (toSql <$> as)  <> ")"
  toSql (SqlESelect s) = toSql s
  toSql (SqlECase b ws e) = "case "
                            <> maybe "" ((<> " ") . toSql) b
                            <> concatMap (\(w, t) -> "when " <> w <> " then " <> toSql t <> " ") ws
                            <> maybe "" ((<> " ") . toSql) e
                            <> "end"
  toSql (SqlECast v t) = "cast(" <> toSql v <> " as " <> toSql v <> ")"
  toSql (SqlEUnaryOperator o v) = o <> toSql v
  toSql (SqlEBinaryOperator a1 o a2) = toSql a1 <> " " <> o <> " " <> toSql a2
  toSql (SqlEAny []) = "*"
  toSql (SqlEAny xs) = intercalate "." (toSql <$> xs)  <> ".*"


-- | Возвращает имя идентификатора если это простой идентификатор (без префикса)
simpleIdentifierName :: SqlIdentifier -> Maybe String
simpleIdentifierName (SqlIdentifier [] (SqlSingleIdentifier a)) = Just (toUpper <$> a)
simpleIdentifierName (SqlIdentifier [] (SqlSingleIdentifierQuoted a))= Just a
simpleIdentifierName _ = Nothing
