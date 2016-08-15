--- Given Code
--- ==========

module Main where

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

data Exp = IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LamExp String Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (AppExp f e)     = show f ++ " " ++ show e
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Integer
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

var :: Parser String
var = let keywords = ["if", "then", "else"]
      in  try $ do v1 <- letter                  <?> "an identifier"
                   vs <- many (letter <|> digit) <?> "an identifier"
                   spaces
                   let v = v1:vs
                   if (any (== v) keywords)
                    then fail "keyword"
                    else return v

oper :: Parser String
oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

--- ### Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: String -> Parser (Exp -> Exp -> Exp)
opExp str = do symbol str
               return (OpExp str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = opExp "*" <|> opExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = opExp "+" <|> opExp "-"

compOp :: Parser (Exp -> Exp -> Exp)
compOp =     opExp "<"  <|> opExp ">"
         <|> opExp "<=" <|> opExp ">="
         <|> opExp "/=" <|> opExp "=="

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp :: Parser Exp
lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

appExp :: Parser Exp
appExp = do e1 <- expr
            e2 <- expr
            return $ AppExp e1 e2

atom :: Parser Exp
atom =     intExp
       <|> ifExp
       <|> lamExp
       <|> varExp
       <|> parens expr

expr :: Parser Exp
expr = let arith  = term `chainl1` addOp
           term   = factor `chainl1` mulOp
           factor = app
           app    = do f <- many1 atom
                       return $ foldl1 AppExp f
       in  arith `chainl1` compOp

parseExp :: String -> Either ParseError Exp
parseExp str = parse expr "stdin" str

--- ### Declarations

decl :: Parser Stmt
decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseDecl :: String -> Either ParseError Stmt
parseDecl str = parse decl "stdin" str

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\r -> k $ r * n)

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t 
evenoddk (x:[]) k1 k2 
  | mod x 2 == 0 = k1 x
  | otherwise    = k2 x
evenoddk (x:xs) k1 k2 
  | mod x 2 == 0 = evenoddk xs (\r -> k1 $ r+x) (\r -> k2 $ r+0)
  | otherwise    = evenoddk xs (\r -> k1 $ r+0) (\r -> k2 $ r+x)

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### `isSimple :: Exp -> Bool`

isSimple :: Exp -> Bool
isSimple (IntExp ex) = True 
isSimple (VarExp ex) = True 
isSimple (LamExp s  ex)   = False
isSimple (AppExp e1 e2)   = False
isSimple (IfExp e1 e2 e3) = (isSimple e1) && (isSimple e2) && (isSimple e3)
isSimple (OpExp f  e1 e2) = (isSimple e1) && (isSimple e2)

--- ### `cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)`

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e1) k n = 
  case isSimple e1 of 
    True  -> (AppExp (AppExp f e1) k, n)
    False ->
      let 
        (v, n1) = gensym n
      in 
        cpsExp e1 (LamExp v (AppExp (AppExp f (VarExp v)) k)) n1

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n = 
  case isSimple e1 of
    True  -> 
      let 
        (ne2, n2) = (cpsExp e2 k n)
        (ne3, n3) = (cpsExp e3 k n2)
      in 
        (IfExp e1 ne2 ne3, n3)
    False -> 
      let 
        (v, n1) = gensym n
        (ne2, n2) = (cpsExp e2 k n1)
        (ne3, n3) = (cpsExp e3 k n2)
      in 
        cpsExp (e1) (LamExp v (IfExp (VarExp v) ne2 ne3)) n3
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp f e1 e2) k n = 
  case (isSimple e1, isSimple e2) of 
    (True,  True)  -> (AppExp k (OpExp f e1 e2), n)
    (True,  False) -> 
      let (v, n2) = gensym n
      in cpsExp e2 (LamExp v (AppExp k (OpExp f e1 (VarExp v)))) n2
    (False, True)  ->
      let (v, n2) = gensym n 
      in cpsExp e1 (LamExp v (AppExp k (OpExp f (VarExp v) e2))) n2
    (False, False) -> 
      let 
        (v1, n1) = gensym n
        (v2, n2) = gensym n1 
        (ne2, n3) = cpsExp e2 (LamExp v2 (AppExp k (OpExp f (VarExp v1) (VarExp v2)))) n2
      in 
        cpsExp e1 (LamExp v1 ne2) n3

cpsExp _ k n = (k, n)
--- ### `cpsDecl :: Stmt -> Stmt`
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f ss e1) = 
  let (e, n) = cpsExp e1 (VarExp "k") 0
  in Decl f (ss++["k"]) e
