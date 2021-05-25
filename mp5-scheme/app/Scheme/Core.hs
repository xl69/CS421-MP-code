{-# LANGUAGE FlexibleContexts #-}

--- Given Code
--- ==========

module Scheme.Core where

import Prelude hiding (lookup)
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Data.Typeable
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

--- ### Environment
type Env = H.HashMap String Val

--- ### Value
data Val = Symbol String
         | Boolean Bool
         | Number Int
         | Nil     
         | Pair Val Val   
         | PrimFunc ([Val] -> EvalState Val)  -- Primitive func impl'ed in Haskell
         | Func [String] Val Env              -- Closure
         | Macro [String] Val                 -- Macro
         | Void                               -- No value
         deriving (Typeable)

-- Flatten dotted lists to avoid complex cases
-- Examples:
--   flattenList (. 4)  ==> 4
--   flattenList (1 . (1 2))  ==> (1 1 2)
--   flattenList (1 . (1 . (1 2 3 4)))  ==> (1 1 1 2 3 4)
--   flattenList (1 . (1 . (1 2 3 4 . 5)))  ==> (1 1 1 2 3 4 . 5)
--   flattenList (1 . (1 . 2))  ==> (1 1 . 2)
--   flattenList ((1 . 1) . 2)  ==> ((1 . 1) . 2)
flattenList :: Val -> Either ([Val], Val) [Val]
flattenList (Pair v1 v2) =
  case flattenList v2 of
    Right vl -> Right (v1 : vl)
    Left (p1, p2) -> Left (v1 : p1, p2)
flattenList Nil = Right []
flattenList v = Left ([], v)

instance Show Val where
  show (Symbol sym)     = sym
  show (Nil)            = "()"
  show (Pair v1 v2)     = 
    case flattenList (Pair v1 v2) of
      Right vl -> "(" ++ unwords (map show vl) ++ ")"
      Left (p1, p2) -> "(" ++ unwords (map show p1) ++ " . " ++ show p2 ++ ")" 
  show (Number i)       = show i
  show (Boolean b)      = if b then "#t" else "#f"
  show (PrimFunc _)     = "#<primitive>"
  show (Func args _ _)  = "#<function:(λ (" ++ unwords args ++ ") ...)>"
  show (Macro args _)   = "#<macro (" ++ unwords args ++ ") ...>"
  show Void             = ""

showArgs :: [Val] -> String
showArgs = unwords . map show

typeName :: Val -> String
typeName Symbol{} = "Symbol"
typeName Pair{} = "Pair"
typeName Nil{} = "Nil"
typeName Number{} = "Number"
typeName Boolean{} = "Boolean"
typeName PrimFunc{} = "PrimFunc"
typeName Func{} = "Func"
typeName Macro{} = "Macro"
typeName Void = "Void"

--- ### Diagnostic

--- In our monadic evaluator, all you need to throw an diagnostic message is to
--- call `throwError`, e.g. `throwError $ NotASymbol v`. Pick the right diagnosic
--- to throw!
data Diagnostic = UnexpectedArgs [Val]
                | TypeError Val
                | NotFuncError Val
                | UndefSymbolError String
                | NotArgumentList Val
                | InvalidSpecialForm String Val
                | CannotApply Val [Val]
                | InvalidExpression Val
                | NotASymbol Val
                | NotAListOfTwo Val
                | UnquoteNotInQuasiquote Val
                | Unimplemented String

err_type :: Diagnostic -> String
err_type (UndefSymbolError _) = "ERROR: undef_symbol"
err_type (NotASymbol _) = "ERROR: nota_symbol"
err_type (InvalidSpecialForm _ _) = "ERROR: invalid_special_form"
err_type (UnquoteNotInQuasiquote _) = "ERROR: unquote_notin_quasiquote"
err_type (UnexpectedArgs _) = "ERROR: unexpected_args"
err_type (TypeError _) = "ERROR: type_error"
err_type _ = "ERROR: other"

instance Show Diagnostic where
  show (UnexpectedArgs actual) =
    "Error: Unexpected arguments or wrong number of arguments (" ++ unwords (map show actual) ++ ")"
  show (TypeError v) =
    "Error: Value " ++ show v ++ " has unexpected type " ++ typeName v
  show (NotFuncError val) =
    "Error: " ++ show val ++ " is not a function"
  show (UndefSymbolError name) =
    "Error: Symbol " ++ name ++ " is undefined"
  show (NotArgumentList val) =
    "Error: Expecting an argument list, but found " ++ show val
  show (InvalidSpecialForm f val) =
    "Error: Invalid pattern in special form `" ++ f ++ "`: " ++ show val
  show (CannotApply val args) =
    "Error: Cannot apply " ++ show val ++ " on argument list (" ++ unwords (map show args) ++ ")"
  show (InvalidExpression val) =
    "Error: Invalid expression: " ++ show val
  show (NotASymbol val) =
    "Error: Not a symbol: " ++ show val
  show (NotAListOfTwo val) =
    "Error: Not a list of two elements: " ++ show val
  show (UnquoteNotInQuasiquote val) =
    "Error: `unquote` not in a `quasiquote` context: " ++ show val
  show (Unimplemented feature) =
    "Error: " ++ feature ++ " is not implemented. You should implement it first!"

-- ### Evaluation monad

-- `StateT` is the monad transformer version of `State`. You do not need to
-- understand monad transformers! Simply read the following declaration as:
-- `EvalState` is a state encapsulating the evaluation result of type `a` and
-- the environment of type `Env`, except when a `Diagnostic` is thown along the
-- evaluation
type EvalState a = StateT Env (Except Diagnostic) a

-- Throw an 'Unimplemented' error with a feature name
unimplemented :: String -> EvalState a
unimplemented = throwError . Unimplemented
