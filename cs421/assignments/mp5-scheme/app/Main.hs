--- Given Code
--- ==========

module Main where

import Prelude hiding (lookup)
import Data.List (intercalate)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap, fromList, lookup, insert, union, empty)


--- Problems (Part 1)
--- =================

--- Datatypes
--- ---------

--- ### Environments

type Env = HashMap String Val

--- ### Expressions

data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         deriving (Show, Eq)

--- ### Values

data Val = IntVal Integer
         | SymVal String
         | ExnVal String
         | PrimVal ([Val]->Val)
         | Closure [String] Exp Env
         | DefVal String Val
         | ConsVal Val Val 
         | Macro [String] Exp Env
instance Show Val where
    show (IntVal i)      = show i 
    show (SymVal s)      = s
    show (ExnVal s)      = "*** Scheme-Exception: " ++ s ++ " ***"
    show (PrimVal _)     = "*primitive*"
    show (Closure _ _ _) = "*closure*"
    show (Macro _ _ _)   = "*macro*"
    show (DefVal s _)    = s
    show (ConsVal v1 v2) = "(" ++ _show (ConsVal v1 v2) ++ ")"
      where 
        _show (ConsVal v1 (SymVal "nil"))  = show v1 ++ " "
        _show (ConsVal (ConsVal v1 v2) (ConsVal v3 v4)) = show (ConsVal v1 v2) ++ " " ++ _show (ConsVal v3 v4)
        _show (ConsVal (ConsVal v1 v2) v3) = show (ConsVal v1 v2) ++ " " ++ show v3
        _show (ConsVal v1 (ConsVal v2 v3)) = show v1 ++ " " ++ _show (ConsVal v2 v3) 
        _show (ConsVal v1 v2)              = show v1 ++ " . " ++ show v2
--- Parsing
--- -------

type Parser = ParsecT String () Identity

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = parse parser "" input

--- ### Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

--- #### Whitespace parser

whitespace :: Parser String
whitespace = many (oneOf " \n\t")

--- #### Identifier parser

identFirst :: Parser Char
identFirst = oneOf ("-*+/:'?><=!" ++ ['a'..'z'] ++ ['A'..'Z'] )

identRest :: Parser Char
identRest  = oneOf ("-*+/:'?><=!" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

identifier :: Parser String
identifier = 
  do 
    x <- identFirst
    xs <- many identRest
    return (x:xs)

--- ### Grammaticals

anInt :: Parser Exp
anInt = do d <- digits
           return $ IntExp (read d)

--- #### Parsing symbols

aSym :: Parser Exp
aSym = 
    do
      s <- identifier 
      return $ SymExp s

--- #### Parsing forms

aForm :: Parser Exp
aForm = 
    do
      Text.ParserCombinators.Parsec.try $ string "("
      exps <- many $ do whitespace
                        e <- anExp 
                        return e 
      whitespace
      string ")"
      return $ SExp exps 

--- #### Quotes, Quasi-Quotes, and UnQuotes

aQuote :: Parser Exp
aQuote = 
    do 
      Text.ParserCombinators.Parsec.try $ whitespace
      string "\'"
      whitespace
      e <- anExp 
      whitespace
      return $ SExp [SymExp "quote", e]

aQQuote :: Parser Exp
aQQuote = 
    do 
      Text.ParserCombinators.Parsec.try $ whitespace
      string "`"
      whitespace
      e <- anExp 
      whitespace
      return $ SExp [SymExp "quasiquote", e]

anUnquote :: Parser Exp
anUnquote = 
  do
    Text.ParserCombinators.Parsec.try $ whitespace
    string ","
    whitespace
    e <- anExp
    whitespace 
    return $ SExp [SymExp "unquote", e]

--- optionally, the above can be defined in terms of `mkQuote`
mkQuote :: Char -> String -> Parser Exp
mkQuote = undefined

anyQuote :: Parser Exp
anyQuote = 
  do   
    whitespace
    d <- aQuote <|> aQQuote <|> anUnquote
    whitespace
    return $ d

--- #### Expression Parser

anExp :: Parser Exp
anExp = 
  do
    whitespace
    d <- anInt <|>  aForm <|> anyQuote <|> aSym
    whitespace
    return $ d

--- Lifters/Lowerers
--- ----------------

liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

--- ### Boolean operations

liftBoolOp :: ([Bool] -> Bool) -> ([Val] -> Val)
liftBoolOp f l = liftbool $ f (map lowerbool l) 

--- ### Integer operations

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> ([Val] -> Val)
liftIntOp f i [] = liftint i 
liftIntOp f i l  = 
  let 
    (x:xs) = map lowerint l 
    sum    = foldl f x xs 
  in 
    liftint sum 

--- ### Comparison operations

liftCompOp :: (Integer -> Integer -> Bool) -> ([Val] -> Val)
liftCompOp f = aux f True 
  where
    aux :: (Integer->Integer->Bool)->Bool->[Val]->Val 
    aux f b (x1:x2:[]) = liftbool $ (f (lowerint x1) (lowerint x2)) && b 
    aux f b (x1:x2:xs) = aux f ((f (lowerint x1) (lowerint x2)) && b) (x2:xs)
    aux f b _          = liftbool b

--- ### List operations

liftList :: [Val] -> Val
liftList []     = liftbool False
liftList (x:xs) = ConsVal x (liftList xs)

lowerList :: Val -> [Val]
lowerList (ConsVal v1 v2) = v1 : lowerList v2 
lowerList (SymVal "nil")  = [] 
lowerList v               = error $ show v 

--- Problems (Part 2)
--- =================

--- Runtime
--- -------

runtime :: Env
runtime = foldl union empty [ runtimeArith
                            , runtimeComp
                            , runtimeBool
                            , runtimeUnary
                            , runtimeOther
                            ]

withRuntime :: String -> String
withRuntime input
    = let (SExp (SymExp op : args)) = (\(Right r) -> r) . parseWith aForm $ input
      in  case lookup op runtime of
            Just (PrimVal f) -> show . f . map (flip eval runtime) $ args
            _                -> error $ "Failed lookup '" ++ show op ++ "' in 'runtime'."

--- ### Arithmetic

runtimeArith :: Env
runtimeArith = fromList [ ("+", PrimVal $ liftIntOp (+) 0)
                        , ("-", PrimVal $ liftIntOp (-) 0)
                        , ("*", PrimVal $ liftIntOp (*) 1)
                        ]

--- ### Comparison

runtimeComp :: Env
runtimeComp = fromList [ (">", PrimVal $ liftCompOp (>))
                       , ("<", PrimVal $ liftCompOp (<))
                       , (">=", PrimVal $ liftCompOp (>=))
                       , ("<=", PrimVal $ liftCompOp (<=))
                       , ("=" , PrimVal $ liftCompOp (==))
                       , ("!=", PrimVal $ liftCompOp (/=))
                       ]

--- ### Boolean Operators

runtimeBool :: Env
runtimeBool = fromList [ ("and", PrimVal $ liftBoolOp and)
                       , ("or" , PrimVal $ liftBoolOp or)
                       ]

--- ### Unary Operators

primNot :: Val -> Val
primNot x = liftbool $ not (lowerbool x)  

primCar :: Val -> Val
primCar (ConsVal car cdr) = car
primCar val               = ExnVal $ "Not a cons cell: " ++ show val

primCdr :: Val -> Val
primCdr (ConsVal car cdr) = cdr
primCdr val               = ExnVal $ "Not a cons cell: " ++ show val  

primUnary :: String -> (Val -> Val) -> [Val] -> Val
primUnary _ f [v] = f v 
primUnary s _ _   = ExnVal $ "`" ++ s ++ "` is a unary operator."

runtimeUnary :: Env
runtimeUnary = fromList [ ("not", PrimVal $ primUnary "not" primNot)
                        , ("car", PrimVal $ primUnary "car" primCar)
                        , ("cdr", PrimVal $ primUnary "cdr" primCdr)
                        ]

--- ### Other operators

primEq :: [Val] -> Val
primEq [] = liftbool True 
primEq vs = 
  liftbool $ and (zipWith eq vs (tail vs))
  where 
    eq (IntVal i1)         (IntVal i2)         = i1 == i2
    eq (SymVal s1)         (SymVal s2)         = s1 == s2
    eq (ConsVal car1 cdr1) (ConsVal car2 cdr2) = (eq car1 car2) && (eq cdr1 cdr2)
    eq _                    _                  = False 

runtimeOther :: Env
runtimeOther = fromList [ ("eq?" , PrimVal $ primEq)
                        , ("list", PrimVal $ liftList)
                        ]

--- Evaluation
--- ----------

--- ### Check parameter names

paramStrs :: [Exp] -> Either String [String]
paramStrs = traverse param 
  where 
      param (SymExp exp) = Right exp
      param _            = Left "Must use only `SymExp` for parameter names."

--- ### Quoting, Quasi-Quoting, and Unquoting

quote :: Exp -> Val
quote (IntExp i)  = IntVal i 
quote (SymExp s)  = SymVal s
quote (SExp exps) = liftList $ map quote exps 

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote (SExp [SymExp "unquote", exp])    env 1 = eval exp env 
quasiquote (SExp [SymExp "unquote", exp])    env i = liftList $ [SymVal "unquote", quasiquote exp env (i-1)]
quasiquote (SExp [SymExp "quasiquote", exp]) env i = liftList $ [SymVal "quasiquote", quasiquote exp env (i+1)]
quasiquote (SExp l )                         env i = liftList $ map (\exp-> quasiquote exp env i) l 
quasiquote exp                               env _ = quote exp

unquote :: Val -> Exp
unquote (IntVal i)      = IntExp i
unquote (SymVal s)      = SymExp s
unquote l@(ConsVal _ _) = SExp (map unquote (lowerList l))

--- ### Evaluation - the function!

eval :: Exp -> Env -> Val
--- #### Integer, Symbol, and Empty Forms
eval (IntExp i) env = IntVal i 

eval (SymExp s) env = 
  case (lookup s env) of 
    Just v  -> v
    Nothing -> ExnVal $ "Symbol" ++ s ++ " has no value."

eval (SExp []) env = liftbool $ False 

--- #### Variable Definition Forms
eval (SExp [SymExp "def", SymExp var, exp]) env = DefVal var $ eval exp env 

--- #### Function Definition and Lambda Function Forms
eval (SExp [SymExp "define", SymExp f, SExp params, exp]) env = 
  case paramStrs params of
    Right ps -> 
      let 
        closure = Closure ps exp env'
        env' = insert f closure env
      in 
        DefVal f closure
    Left err -> ExnVal err 

eval (SExp [SymExp "lambda", SExp params, exp]) env = 
  case paramStrs params of 
    Right ps -> Closure ps exp env
    Left err -> ExnVal err 

--- #### Quoting, Quasi-Quoting, and Unquoting Forms
eval (SExp [SymExp "quote", exp]) env = quote exp 

eval (SExp [SymExp "quasiquote", exp]) env = quasiquote exp env 1

eval (SExp [SymExp "unquote", exp]) env = ExnVal "Cannot `unquote` more than `quasiquote`."

--- #### Conditional Form
eval (SExp [SymExp "cond", SExp exp]) env =
  case exp of 
    (cond : trueB : rest) -> 
      case (lowerbool $ eval cond env) of 
        True  -> eval trueB env 
        False -> eval (SExp [SymExp "cond", SExp rest]) env 
    otherwise             -> liftbool False 

--- #### Let Form
eval (SExp [SymExp "let", SExp defs, body]) env = 
  let 
    defs' = map (\(SExp [SymExp var, val]) -> (var, eval val env)) defs
    env'  = union (fromList defs') env 
  in 
    eval body env' 

--- #### Cons Form
eval (SExp [SymExp "cons", car, cdr]) env = ConsVal (eval car env) (eval cdr env)

--- #### Eval Form
eval (SExp [SymExp "eval", exp]) env = eval (unquote (eval exp env)) env 

--- #### Macro Form
eval (SExp [SymExp "defmacro", SymExp f, SExp params, exp]) env = 
  case paramStrs params of 
    Right ps -> 
      let 
        macro = Macro ps exp env' 
        env'  = insert f macro env 
      in 
        DefVal f macro 
    Left err -> ExnVal err 

--- #### Application Form
eval (SExp (f : as)) env = 
  let
    as' = map (\a->eval a env) as
  in case eval f env of 
    PrimVal pf          -> pf as' 
    Closure ps exp cenv -> 
      let 
        env' = union (fromList $ zip ps as') cenv 
      in 
        eval exp env' 
    Macro ps exp menv   ->
      let 
        env' = union (fromList $ zip ps (map quote as)) menv 
      in 
        eval (unquote (eval exp env')) env 
    a                   -> a

--- REPL
--- ----

--- ### Generating next environment

nextEnv :: Env -> Val -> Env
nextEnv env (DefVal var val) = insert var val env
nextEnv env _                = env   

--- ### Writing the REPL

prompt :: String -> IO String
prompt str = hPutStr stdout str >> hFlush stdout >> hGetLine stdin

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl = undefined

--- ### Main function

main :: IO ()
main = do printLn "Welcome to your Scheme interpreter!"
          repl runtime
          printLn "Goodbye!"
