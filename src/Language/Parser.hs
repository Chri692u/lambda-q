{-# LANGUAGE OverloadedStrings #-}

module Language.Parser where

import Language.Syntax
import Language.Types

import Data.Functor
import qualified Data.Functor
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

-- Top level
parseExp :: Text -> Either (ParseErrorBundle Text Void) Exp
parseExp = parse (spaces *> expr <* eof) ""

parseLine :: Text -> Either (ParseErrorBundle Text Void) Exp
parseLine input = case parseExp input of
        Left err -> Left err
        Right e  -> return e

---------------------------------------------------
-- Lexing
---------------------------------------------------

type Parser = Parsec Void Text

spaces :: Parser ()
spaces = L.space space1 lComment bComment
    where lComment = L.skipLineComment "--"
          bComment = L.skipBlockComment "{-" "-}"

-- Tokens
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

---------------------------------------------------
-- Parser
---------------------------------------------------
constant :: Parser Exp
constant = choice [
    symbol "cwd" $> Con CCWD,
    symbol "cd" *> (Con . CCD <$> expr),
    symbol "touch" *> (Con . CTOUCH <$> expr),
    symbol "mkdir" *> (Con . CMKDIR <$> expr),
    symbol "rm" *> (Con . CRM <$> expr),
    Con . CString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  ]

variable :: Parser Exp
variable = do
    var <- lexeme $ some letterChar
    return $ Var var

abstraction :: Parser Exp
abstraction = do
    symbol "\\"
    var <- lexeme $ some letterChar
    symbol ":"
    t <- types
    symbol "."
    Abs var t <$> expr

{-
data Exp = Var Id
         | Con Con
         | App Exp Exp
         | Abs Id Type Exp
         | Quote Exp
         | Unquote Exp
         | Bin BOps Exp Exp
-}
term :: Parser Exp
term = choice [
        parens expr,
        try constant,
        variable,
        abstraction
    ]

expr :: Parser Exp
expr = do
    es <- many term
    return (foldl1 App es)

-- Type parsers
tAtom :: Parser Type
tAtom = choice [symbol "string" $> stringType,
                symbol "unit" $> unitType,
                parens types]

types :: Parser Type
types = makeExprParser tAtom tyops
  where
    infixOp x f = InfixR (symbol x $> f)
    tyops = [[infixOp "->" TArr]]