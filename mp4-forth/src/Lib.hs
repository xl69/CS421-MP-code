--- xl69, tianhui2
--- Given Library Code
--- ==================

module Lib where

type ErrorMsg = String
-- for stack underflow errors
msgUnderflow :: ErrorMsg
msgUnderflow = "Stack underflow"

-- for definition with no name
msgZeroLenDef :: ErrorMsg
msgZeroLenDef = "Attempt to use zero-length string as a name"

-- for interpreting compile-only words
msgCompileOnly :: ErrorMsg
msgCompileOnly = "Interpreting a compile-only word"

-- for undefined symbol
msgUndefinedSym :: String -> ErrorMsg
msgUndefinedSym name = "Undefined symbol: '" ++ name ++ "'"

-- for unstructured control keywords
msgUnstructured :: String -> ErrorMsg
msgUnstructured op = "Unstructured control: " ++ op

underflow :: a
underflow = error msgUnderflow

--- The Types
--- ---------

type ForthState = (IStack, Dictionary, Output)
type Transition = (ForthState -> ForthState)
type IStack     = [Integer]
type Dictionary = [(String, Value)]
type Output     = [String]

type CStack     = [(String, Transition)]

data Value = Prim Transition
           | Define
           | EndDef
           | Compile (CStack -> Maybe CStack)
           | Num Integer
           | Unknown String

instance Show Value where
    show (Prim f)    = "Prim"
    show (Compile _) = msgCompileOnly
    show Define      = "Define"
    show EndDef      = msgCompileOnly
    show (Num i)     = show i
    show (Unknown s) = msgUndefinedSym s

--- Dictionary Access
--- -----------------

--- ### Lookups

-- handle input lookups (and integers)
dlookup :: String -> Dictionary -> Value
dlookup word dict
    = case lookup word dict of
        Just x -> x
        _          -> case reads word of
                        [(i,"")] -> Num i
                        _        -> Unknown word

--- ### Insert

-- handle inserting things into the dictionary
dinsert :: String -> Value -> Dictionary -> Dictionary
dinsert key val dict = (key, val):dict

--- Problems
--- ========

--- Lifters
--- -------

liftIStackOp :: (IStack -> Maybe IStack) -> ForthState -> ForthState
liftIStackOp op (i, d, o)
    = case op i of
        Just i' -> (i', d, o)
        Nothing -> underflow

--- ### `liftIntOp`

liftIntOp :: (Integer -> Integer -> Integer) -> IStack -> Maybe IStack
liftIntOp op (x:y:xs) = Just $ (y `op` x) : xs
liftIntOp _  _        = Nothing

--- ### `liftCompOp`

liftCompOp :: (Integer -> Integer -> Bool) -> IStack -> Maybe IStack
liftCompOp op (x:y:xs) | (y `op` x) == True = Just $ -1 : xs
                       | (y `op` x) == False = Just $ 0 : xs
liftCompOp _  _        = Nothing


--- The Dictionary
--- --------------

initialDictionary :: Dictionary
initialDictionary = initArith ++ initComp ++ initIStackOp ++ initPrintOp
                 ++ initCompileOp

initCompileOp = [ (":",    Define)
                , (";",    EndDef)
                , ("for",  Compile cstackFor)
                , ("next", Compile cstackNext)
                , ("if",   Compile cstackIf)
                , ("else", Compile cstackElse)
                , ("then", Compile cstackThen)
                , ("begin",Compile cstackBegin)
                , ("until",Compile cstackUntil)
                ]

--- ### Arithmetic Operators

initArith :: Dictionary
initArith = [ ("+",  Prim $ liftIStackOp $ liftIntOp (+)),
              ("-",  Prim $ liftIStackOp $ liftIntOp (-)),
              ("*",  Prim $ liftIStackOp $ liftIntOp (*)),
              ("/",  Prim $ liftIStackOp $ liftIntOp (div))
            ]

--- ### Comparison Operators

initComp :: Dictionary
initComp = [ (">",  Prim $ liftIStackOp $ liftCompOp (>)),
             (">=",  Prim $ liftIStackOp $ liftCompOp (>=)),
             ("<",  Prim $ liftIStackOp $ liftCompOp (<)),
             ("<=",  Prim $ liftIStackOp $ liftCompOp (<=)),
             ("=",  Prim $ liftIStackOp $ liftCompOp (==)),
             ("!=",  Prim $ liftIStackOp $ liftCompOp (/=))
            ]

--- ### Stack Manipulations

initIStackOp :: Dictionary
initIStackOp = [ ("dup",  Prim $ liftIStackOp istackDup),
                 ("swap",  Prim $ liftIStackOp istackSwap),
                 ("drop",  Prim $ liftIStackOp istackDrop),
                 ("rot",  Prim $ liftIStackOp istackRot)
               ]

initPrintOp = [ (".",  Prim printPop),
                (".S",  Prim printStack)
              ]

istackDup :: IStack -> Maybe IStack
istackDup (i:is) = Just $ i:i:is
istackDup _      = Nothing

istackSwap :: IStack -> Maybe IStack
istackSwap (x:y:xs) = Just (y:x:xs)
istackSwap _      = Nothing

istackDrop :: IStack -> Maybe IStack
istackDrop (x:xs) = Just xs
istackDrop _      = Nothing

istackRot :: IStack -> Maybe IStack
istackRot (x1:x2:x3:xs) = Just (x3:x1:x2:xs)
istackRot _      = Nothing

--- ### Popping the Stack

printPop :: ForthState -> ForthState
printPop (i:istack, dict, out) =
    (istack, dict, show i : out)
printPop _ = underflow

--- ### Printing the Stack

printStack :: ForthState -> ForthState
printStack (istack, dict, out) = (istack, dict, (unwords $ map show (reverse istack)):out)

--- Evaluator
--- ---------

eval :: [String] -> ForthState -> ForthState

-- empty input -> return current state and output "ok"
eval [] (istack, dict, out) = (istack, dict, "ok":out)

--- ### Lookup in dictionary

-- otherwise it should be handled by `dlookup` to see if it's a `Num`, `Prim`,
-- `Define`, `Compile`, or `Unknown`
eval (w:ws) state@(istack, dict, out)
    = case dlookup w dict of
        Prim f    -> eval ws (f state)
        Num i     -> eval ws (i:istack, dict, out)
        Define    ->
            case compileDef ws dict of
                Right (rest, dict') -> eval rest (istack, dict', out)
                Left msg -> ([], dict, msg:out)
        otherwise -> -- reset IStack and add error message
            ([], dict, (show otherwise):out)

--- Compiler
--- --------

--- ### Definite Loops
transForLoop :: Transition -> (ForthState -> ForthState)
transForLoop kloop (i:is, d, o) =
    if i < 0 then
        (is, d, o)
    else
        (aux kloop i) (is, d, o)
        where aux k 0 = k
              aux k n = k . (aux k (n-1))
transForLoop _ _ = underflow


cstackFor :: CStack -> Maybe CStack
cstackFor cstack = Just $ ("for", id):cstack

cstackNext :: CStack -> Maybe CStack
cstackNext (("for", kloop):(c, kold):cstack) =
    Just ((c, knew):cstack) where knew = (transForLoop kloop) . kold
cstackNext _ = Nothing

--- ### Conditionals

transIf :: Transition -> (ForthState -> ForthState)
transIf kif (i:is, d, o) = 
    if i == 0 then
        (is, d, o)
    else 
        kif (is, d, o)

transElse :: Transition -> Transition -> (ForthState -> ForthState)
transElse kif kelse (i:is, d, o) = 
    if i == 0 then
        kelse (is, d, o)
    else 
        kif (is, d, o)

cstackIf :: CStack -> Maybe CStack
cstackIf cstack = Just $ ("if", id):cstack 

cstackElse :: CStack -> Maybe CStack
cstackElse cstack@(("if", _):_) = Just $ ("else", id):cstack 
cstackElse _ = Nothing

cstackThen :: CStack -> Maybe CStack
cstackThen (("else", kelse):("if", kif):(c, kold):cstack) = 
    Just ((c, knew):cstack) where knew = (transElse kif kelse) . kold
cstackThen (("if", kif):(c, kold):cstack) = 
    Just ((c, knew):cstack) where knew = (transIf kif) . kold
cstackThen _ = Nothing

--- ### Indefinite Loops

transUntil :: Transition -> (ForthState -> ForthState)
transUntil kloop (is, d, o) = 
    let (nexti:nextis, nextd, nexto) = kloop (is, d, o) in
    if nexti == 0 then
        transUntil kloop (nextis, nextd, nexto)
    else 
        (nextis, nextd, nexto)

cstackBegin :: CStack -> Maybe CStack
cstackBegin cstack = Just $ ("begin", id):cstack

cstackUntil :: CStack -> Maybe CStack
cstackUntil (("begin", kloop):(c, kold):cstack) = 
    Just ((c,knew):cstack) where knew = (transUntil kloop) . kold
cstackUntil _ = Nothing


--- ### Lookup in dictionary

-- | Return rest of words after compilation and a dictionary w/ new defintion
-- Assuming ':' is already stripped away
compileDef :: [String] -> Dictionary -> Either ErrorMsg ([String], Dictionary)
compileDef [] _ = Left msgZeroLenDef
compileDef (name:ws) dict
    = case  compile ws dict [("", id)] of
       Right (rest, f) -> Right (rest, dinsert name (Prim f) dict)
       Left msg -> Left msg

compile :: [String] -> Dictionary -> CStack
           -> Either ErrorMsg ([String], Transition)
compile [] _ _ = Left "The definition does not end"

compile (w:ws) dict cstack
    = case dlookup w dict of
        Prim f    -> compile ws dict (updateTop f cstack)
        Num i     -> compile ws dict (updateTop f cstack)
                     where f = (liftIStackOp (\is -> Just (i:is)))
        Compile cf-> case cf cstack of
                Just cstack' -> compile ws dict cstack'
                Nothing -> Left $ msgUnstructured w
        Define    -> Left "Nested definition is not allowed"
        EndDef    -> case cstack of
            [("", k)] -> Right (ws, k)
            otherwise -> Left $ msgUnstructured w
        otherwise -> Left $ show otherwise

updateTop :: Transition -> CStack -> CStack
updateTop k ((c, kold):cs) = (c, k . kold) : cs
updateTop _ [] = underflow

