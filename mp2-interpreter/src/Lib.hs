module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of Just n -> n
                                             Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env 
    | op == "/" && (eval e2 env) == IntVal 0 = ExnVal "Division by 0"
    | otherwise = liftIntOp realOp (eval e1 env) (eval e2 env)
                            where realOp = case H.lookup op intOps of Just n -> n

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = liftBoolOp realOp (eval e1 env) (eval e2 env)
                            where realOp = case H.lookup op boolOps of Just n -> n

eval (CompOpExp op e1 e2) env = liftCompOp realOp (eval e1 env) (eval e2 env)
                            where realOp = case H.lookup op compOps of Just n -> n

--- ### If Expressions

eval (IfExp e1 e2 e3) env 
    | eval e1 env == BoolVal True = eval e2 env
    | eval e1 env == BoolVal False = eval e3 env
    | otherwise = ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    let argument = map(\x -> eval x env) args
    in case eval e1 env of CloVal params body env_closure -> eval body $ H.union (H.fromList(zip params argument)) env_closure
                           _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = eval body (H.union new_hash env)
                               where new_hash = H.fromList(map (\(key, value) -> (key, eval value env)) pairs)


--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, (H.insert key value env))
                                where key = var
                                      value = eval e env

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (first ++ remain, pren_2, env_2)
                                 where (first, pren_1, env_1) = exec x penv env
                                       (remain, pren_2, env_2) = exec (SeqStmt xs) pren_1 env_1

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env
    | eval e1 env == BoolVal True = exec s1 penv env
    | eval e1 env == BoolVal False = exec s2 penv env
    | otherwise = (show $ ExnVal "Condition is not a Bool", penv, env) 

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = 
    let argument = map(\x -> eval x env) args
    in case H.lookup name penv of Just (ProcedureStmt _ _args _body) -> exec _body penv (H.union (H.fromList (zip _args argument)) env)
                                  Nothing -> ("Procedure" ++ name ++ "undefined", penv, env)
