module Main where

import System.IO (hFlush, stdout)

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

import Lib

--- Given Code
--- ==========

--- Parser
--- ------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var :: Parser String
var = do v <- many1 letter <?> "an identifier"
         spaces
         return v

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: (String -> Exp -> Exp -> Exp) -> String -> Parser (Exp -> Exp -> Exp)
opExp ctor str = symbol str >> return (ctor str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = let mulOpExp = opExp IntOpExp
        in  mulOpExp "*" <|> mulOpExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = let addOpExp = opExp IntOpExp
        in  addOpExp "+" <|> addOpExp "-"

andOp :: Parser (Exp -> Exp -> Exp)
andOp = opExp BoolOpExp "and"

orOp :: Parser (Exp -> Exp -> Exp)
orOp = opExp BoolOpExp "or"

compOp :: Parser (Exp -> Exp -> Exp)
compOp = let compOpExp s = symbol s >> return (CompOpExp s)
         in     try (compOpExp "<=")
            <|> try (compOpExp ">=")
            <|> compOpExp "/="
            <|> compOpExp "=="
            <|> compOpExp "<"
            <|> compOpExp ">"

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp :: Parser Exp
funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp :: Parser Exp
letExp = do try $ symbol "let"
            symbol "["
            params <- (do v <- var
                          symbol ":="
                          e <- expr
                          return (v,e)
                      )
                      `sepBy` (symbol ";")
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp :: Parser Exp
appExp = do try $ symbol "apply"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr :: Parser Exp
expr = let disj = conj `chainl1` andOp
           conj = arith `chainl1` compOp
           arith = term `chainl1` addOp
           term = factor `chainl1` mulOp
           factor = atom
       in  disj `chainl1` orOp

atom :: Parser Exp
atom = intExp
   <|> funExp
   <|> ifExp
   <|> letExp
   <|> try boolExp
   <|> appExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt :: Parser Stmt
quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt :: Parser Stmt
printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

setStmt :: Parser Stmt
setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt :: Parser Stmt
ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt :: Parser Stmt
procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt :: Parser Stmt
callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt :: Parser Stmt
seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt :: Parser Stmt
stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> try setStmt

--- REPL
--- ----

repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     hFlush stdout
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main :: IO Result
main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"

