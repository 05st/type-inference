{-# Language PatternSynonyms #-}

module Main where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Expr

------------
-- Syntax --
------------

type Name = String
data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Name Expr
    | EApp Expr Expr
    | ELet Name Expr Expr
    | EInt Integer
    | EBool Bool
    | EBin Oper Expr Expr
    deriving Show
data Oper = Add | Sub deriving Show

newtype TVar = TV Name deriving Show
data Type
    = TCon Name [Type]
    | TVar TVar
    deriving Show

pattern TInt = TCon "Int" []
pattern TBool = TCon "Bool" []
pattern a :-> b = TCon "->" [a, b]

------------
-- Parser --
------------

operTable :: OperatorTable String () Identity Expr
operTable = [[Infix (spaces *> char '+' <* spaces >> return (EBin Add)) AssocLeft
            , Infix (spaces *> char '-' <* spaces >> return (EBin Sub)) AssocLeft]]

parseExpr :: Parser Expr
parseExpr = buildExpressionParser operTable parseTerm

parseTerm :: Parser Expr
parseTerm = try parseApp <|> parseIf <|> parseAbs <|> parseLet <|> parseLit 

parseIf :: Parser Expr
parseIf = do
    string "if" *> many1 space
    a <- parseExpr
    many1 space *> string "then" *> many1 space
    b <- parseExpr
    many1 space *> string "else" *> many1 space
    EIf a b <$> parseExpr

parseAbs :: Parser Expr
parseAbs = do
    char '\\' *> spaces
    var <- many1 alphaNum
    spaces *> string "->" *> spaces
    EAbs var <$> parseExpr

parseApp :: Parser Expr
parseApp = do
    fnexpr <- parseLit
    many1 space
    EApp fnexpr <$> parseExpr

parseLet :: Parser Expr
parseLet = do
    string "let"
    many1 space
    var <- many1 alphaNum
    spaces *> char '=' *> spaces
    expr <- parseExpr
    spaces *> string "in" *> many1 space
    ELet var expr <$> parseExpr

parseLit :: Parser Expr
parseLit = (EInt . read <$> many1 digit)
        <|> (EBool True <$ string "True") <|> (EBool False <$ string "False")
        <|> (char '(' *> parseExpr <* char ')')
        <|> (EVar <$> many1 alphaNum)

runParse :: String -> Either String Expr
runParse input =
    case parse parseExpr "input" input of
        Left err -> Left $ show err
        Right expr -> Right expr

----------
-- Main --
----------

main :: IO ()
main = getLine >>= print . runParse >> main
