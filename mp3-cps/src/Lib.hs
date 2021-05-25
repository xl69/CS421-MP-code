--- xl69, tianhui2
--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`
--- Base case: 1; Func: x * (x - 1)

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\res -> k (n * res))


--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x:[]) kaa kbb
    | even x == True = kaa x
    | odd x == True = kbb x
evenoddk (x:xs) kaa kbb
    | even x == True = evenoddk xs (\res -> kaa (res + x)) kbb
    | odd x == True = evenoddk xs kaa (\res -> kbb (res + x))
    
--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (LamExp _ _) = True
isSimple (IfExp cond e1 e2) = isSimple cond && isSimple e1 && isSimple e2
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (AppExp _ _) = False


--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp p@(IntExp i) k f = (AppExp k p, f)
cpsExp p@(VarExp i) k f = (AppExp k p, f)

--- #### Define `cpsExp` for Application Expressions
--- λv.f v k

cpsExp p@(AppExp f e) k shadow = 
    let (v, fresh) = gensym shadow 
    in case isSimple e of
    True -> (AppExp p k, shadow)
    False -> cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) fresh


--- #### Define `cpsExp` for Operator Expressions
cpsExp p@(OpExp op e1 e2) k shadow = 
    let (v, f) = gensym shadow
        (v1, f1) = gensym f 
        (v2, f2) = cpsExp e2 (LamExp v1 (AppExp k (OpExp op (VarExp v) (VarExp v1)))) f1
        var = (VarExp v)
    in case (isSimple e1, isSimple e2) of
        (True, True) -> (AppExp k p, shadow)
        (True, False) -> cpsExp e2 (LamExp v (AppExp k (OpExp op e1 var))) f
        (False, True) -> cpsExp e1 (LamExp v (AppExp k (OpExp op var e2))) f
        (False, False) -> cpsExp e1 (LamExp v v2) f2 
            

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp cond e1 e2) k shadow =
    case isSimple cond of
        True -> let (v1, f1) = cpsExp e1 k shadow
                    (v2, f2) = cpsExp e2 k f1
                in (IfExp cond v1 v2, f2)
        False -> let (v1, f1) = gensym shadow
                     (v2, f2) = cpsExp e1 k f1
                     (v3, f3) = cpsExp e2 k f2
                     var = VarExp v1
                 in cpsExp cond (LamExp v1 (IfExp var v2 v3)) f3

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) = let (v, _) = cpsExp body (VarExp "k") 1
                               in Decl f (params ++ ["k"]) v
