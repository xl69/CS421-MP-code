-- tianhui2, xl69
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
-- TODO: What's self-evaluating?
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
-- TODO
eval (Symbol sym) = do env <- get 
                       case H.lookup sym env of
                            Just v -> return v
                            Nothing -> throwError $ UndefSymbolError sym

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax
    evalList ((Symbol "cond") : statement) = evalCond statement where
      evalCond [] = invalidSpecialForm "cond"
      evalCond (start : rest) = do (s, sigma) <- getListOf2 start
                                   case s of Symbol "else" -> if length rest == 0 then eval sigma
                                                              else invalidSpecialForm "cond"
                                             _ -> do val <- eval s
                                                     case val of Boolean False -> if length rest == 0 then return Void
                                                                                  else evalCond rest
                                                                 _ -> eval sigma 
    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax
    evalList [Symbol "let", args, body] = do env <- get
                                             argList <- getList args
                                             args <- mapM getBinding argList
                                             modify $ H.union $ H.fromList args
                                             val <- eval body
                                             put env
                                             return val

    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
    evalList [Symbol "lambda", args, body] = do env <- get
                                                argList <- getList args
                                                val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
                                                return val

    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- define variable
    -- TODO: Handle `define` for variables here. Use pattern matching
    -- to match the syntax
    evalList [Symbol "define", Symbol vname, body] =
      do val <- eval body
         modify $ H.insert vname val
         return Void

    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    evalList [Symbol "define-macro", Pair (Symbol fname) args, body] =
      do argList <- getList args
         val <- (\argVal -> Macro argVal body) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
  -- Function
    -- TODO: implement function application
    -- Use do-notation!
apply (Func fname body fenv) args = 
  do env <- get
     args <- mapM eval args
     modify $ H.union fenv
     modify $ H.union $ H.fromList $ zip fname args
     val <- eval body
     put env
     return val

  -- Macro
    -- TODO: implement macro evaluation
    -- Use do-notation!
apply (Macro fname body) args = 
  do env <- get
     modify $ H.union $ H.fromList $ zip fname args
     val <- eval body
     put env
     toReturn <- eval val
     return toReturn

  -- Primitive
apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals
  -- Other values are not applicable
apply f args = throwError $ CannotApply f args
