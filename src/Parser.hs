{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
    (
        someFunc,
        myparsetest,
        myrunparser

)

where
import Control.Monad.Combinators.Expr
import DataStructures
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import Data.Void



someFunc = putStrLn "hello world"

type Parser = Parsec Void Text

sc::Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "*/" "*/")
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol = L.symbol sc

myparsetest = parseTest (string "foo" :: Parser Text) "foo"


pPlus :: Parser CExpr
pPlus = do
    symbol "("
    expr1 <- pExpr
    symbol "+"
    expr2 <- pExpr
    symbol ")"
    return (MkPlus expr1 expr2)
pVarname:: Parser CExpr
pVarname= do
    varn<- lexeme (some alphaNumChar)
    return (MkVarE varn)
pInt::Parser CExpr
pInt = do
    MkInt <$>  lexeme L.decimal

pArgs :: Parser CArgs 
pArgs = do 
    args <- sepBy1 pExpr (symbol ",")
    return (MkListArgs args)
pApplyFunc ::Parser CExpr
pApplyFunc = do 
    
    namefunc<-  (lexeme (some alphaNumChar))
    symbol "("
    args<-pArgs
    symbol ")"
    return (MkApplyFunc namefunc args)
pBool= pTrue <|> pFalse

pTrue :: Parser CExpr
pTrue = do
    symbol "true"
    return MkTrue
pFalse :: Parser CExpr
pFalse = do
    symbol "false"
    return MkFalse

pExpr :: Parser CExpr
pExpr =  pInt<|> pBool <|>pPlus <|> try pApplyFunc<|> pVarname


pReturnStatement :: Parser CStatement
pReturnStatement = do
        _ <- symbol "return"
        expr<-  pExpr
        return (ReturnStatement expr)
pEitherDeclorStatement::Parser (Either CDeclaration CStatement)
pEitherDeclorStatement = do
        eitherP pDecl pStatementNoCompound
pCompoundStatement::Parser CStatement
pCompoundStatement = do 
        res <- sepBy1 pEitherDeclorStatement (symbol ";")
        return (Compoundstatement res)
pExprStatement :: Parser CStatement
pExprStatement = do 
    expr<- pExpr
    return (ExprStatement expr)
pStatementNoCompound:: Parser CStatement
pStatementNoCompound = try pReturnStatement     <|>  pExprStatement 

pTyBool :: Parser TypeStruct
pTyBool = do
    symbol "bool"
    return TyBool

pTyInt :: Parser TypeStruct
pTyInt = do
    symbol "int"
    return TyInt

pTypes::Parser TypeStruct
pTypes = choice [pTyBool,pTyInt]
pVarNameAndType :: Parser VariableTypeAndName
pVarNameAndType = do 
    typeofvar<-pTypes
    name <- lexeme (some alphaNumChar)
    return (MkVarInfo name typeofvar)

pFuncDecl :: Parser CDeclaration
pFuncDecl = do
    symbol "function"
    funcname <- pVarNameAndType
    symbol "("
    types <- sepBy pVarNameAndType (symbol ",") 
    symbol ")"
    symbol "{"
    statements <- pCompoundStatement
    symbol "}"
    return (MkFunction funcname types statements)

pFuncExternDecl :: Parser CDeclaration
pFuncExternDecl = do
    symbol "extern"
    symbol "function"
    funcname <- pVarNameAndType
    symbol "("
    types <- sepBy pVarNameAndType (symbol ",") 
    symbol ")"
    symbol ";"
    return (MkExternFunc funcname types)
pVarDecl :: Parser CDeclaration
pVarDecl = do 
    varn <- pVarNameAndType
    symbol "="
    expr<- pExpr
    return (MkVarD varn expr)
pDecl :: Parser CDeclaration
pDecl = pFuncDecl <|> pFuncExternDecl <|> pVarDecl

pProgram :: Parser MyProgram
pProgram = do 
    decls <- some pDecl
    return (MkProgram decls)
myrunparser = parseMaybe pProgram