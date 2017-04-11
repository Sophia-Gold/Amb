{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import System.IO
import System.Environment
import Data.List
import Data.Maybe
import Data.Bool
import Control.Applicative
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Text.Show.Functions


data MultiList = SList [String]
               | CList [Char]
               | IList [Integer]
               | FList [Double] deriving (Eq)

instance Show MultiList where
  show (SList x) = show x
  show (CList x) = show x
  show (IList x) = show x
  show (FList x) = show x

data Amb = Amb { ambTag :: Maybe String, ambVal :: MultiList } deriving (Show)

parseSList :: Exp -> Maybe [String]
parseSList (List exprs) = mapM (\ expr -> case expr of
  Lit (String a) -> Just a
  _              -> Nothing) exprs

parseCList :: Exp -> Maybe [Char]
parseCList (List exprs) = mapM (\ expr -> case expr of
  Lit (Char a) -> Just a
  _            -> Nothing) exprs

parseIList :: Exp -> Maybe [Integer]
parseIList (List exprs) = mapM (\ expr -> case expr of
  Lit (Int a) -> Just a
  _           -> Nothing) exprs

parseFList :: Exp -> Maybe [Double]
parseFList (List exprs) = mapM (\ expr -> case expr of
  Lit (Frac a) -> Just $ fromRational a
  _            -> Nothing) exprs

parseList :: Exp -> MultiList
parseList l = fromJust (SList <$> parseSList l
                    <|> CList <$> parseCList l
                    <|> IList <$> parseIList l 
                    <|> FList <$> parseFList l)

parseAmb :: Maybe String -> Exp -> Amb
parseAmb tag value = Amb tag $ parseList value
  
------------------------------------------------------------------

data AmbVal = StringVal String
            | CharVal Char
            | IntVal Integer
            | FloatVal Double
            | BoolVal Bool
            | ListVal [MultiList] deriving (Eq)

instance Show AmbVal where
  show (StringVal x) = show x
  show (CharVal   x) = show x
  show (IntVal    x) = show x
  show (FloatVal  x) = show x
  show (BoolVal   x) = show x
  show (ListVal   x) = show x

deriving instance Show Require
data Require = Require { reqTag :: Maybe String, reqAmbVal :: AmbVal -> Maybe AmbVal }

parseLiteralString :: String -> Maybe AmbVal
parseLiteralString v = Just $ StringVal v

parseLiteralChar :: Char -> Maybe AmbVal
parseLiteralChar v = Just $ CharVal v

parseLiteralInt :: Integer -> Maybe AmbVal
parseLiteralInt v = Just $ IntVal v

intOp :: (Integer -> Integer -> Integer) -> AmbVal -> AmbVal -> Maybe AmbVal
intOp f (IntVal x) (IntVal y) = Just $ IntVal $ f x y

fracOp :: (Double -> Double -> Double) -> AmbVal -> AmbVal -> Maybe AmbVal
fracOp f (FloatVal x) (FloatVal y) = Just $ FloatVal $ f x y

eqOp :: (forall a. Eq a => a -> a -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
eqOp f x y = Just $ BoolVal $ f x y

ordOp :: (forall a. (Ord a) => a -> a -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
ordOp f (IntVal x) (IntVal y) = Just $ BoolVal $ f x y
ordOp f (FloatVal x) (FloatVal y) = Just $ BoolVal $ f x y

boolOp ::(Bool -> Bool -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
boolOp f (BoolVal x) (BoolVal y) = Just $ BoolVal $ f x y

parseOp :: String -> Maybe (AmbVal -> AmbVal -> Maybe AmbVal)
parseOp op = case op of
  "+"  -> Just $ intOp (+)
  "-"  -> Just $ intOp (-)
  "*"  -> Just $ intOp (*)
  "/"  -> Just $ fracOp (/)
  "/=" -> Just $ eqOp (/=)
  "==" -> Just $ eqOp (==)
  ">"  -> Just $ ordOp (>)
  ">=" -> Just $ ordOp (>=)
  "<"  -> Just $ ordOp (<)
  "<=" -> Just $ ordOp (<=)
  "&&" -> Just $ boolOp (&&)
  "||" -> Just $ boolOp (||)

parseExpr :: String -> Exp -> Maybe (AmbVal -> Maybe AmbVal)
parseExpr var (Var (UnQual (Ident var'))) | var == var' = Just Just
parseExpr var (Lit (String i)) = Just (\_ -> parseLiteralString i)
parseExpr var (Lit (Char i))   = Just (\_ -> parseLiteralChar i)
parseExpr var (Lit (Int i))    = Just (\_ -> parseLiteralInt i)
parseExpr var (List l)         = Just (\_ -> Just $ ListVal [parseList (List l)])
parseExpr var (InfixApp l (QVarOp (UnQual (Symbol sym))) r) = do
  lhs  <- parseExpr var l
  op   <- parseOp sym 
  rhs  <- parseExpr var r
  return $ \val -> do
    lval <- lhs val
    rval <- rhs val
    op lval rval
parseExpr var _ = Nothing

-- error on:    parseLambda "\\x -> x /= [1,2,3]"
parseLambda :: Exp -> Maybe (AmbVal -> Maybe AmbVal) 
parseLambda (Lambda _ [PVar (Ident var)] body) = parseExpr var body

parseRequire :: Maybe String -> Exp -> Require
parseRequire tag value = Require tag $ fromJust $ parseLambda value

--------------------------------------------------------------------

data T = AmbT [Amb]
       | ReqT [Require]
       | EvalT [AmbVal]

parseAST :: String -> Exp
parseAST s = fromParseResult $ parseExp s

amb :: Maybe String -> Exp -> Amb
amb t a = parseAmb t a

require :: Maybe String -> Exp -> Require
require t r =  parseRequire t r

fromJustAmbVal :: Maybe AmbVal -> Bool
fromJustAmbVal m = case m of
  Just (BoolVal True)  -> True
  Just (BoolVal False) -> False
  Nothing              -> undefined

mapMultiList :: (forall a. [a] -> [[a]]) -> MultiList -> [MultiList]
mapMultiList f l = case l of
  SList x -> map SList $ f x
  CList x -> map CList $ f x
  IList x -> map IList $ f x
  FList x -> map FList $ f x

permuteList :: Amb -> [MultiList]
permuteList a = mapMultiList permutations $ ambVal a

eval :: [Amb] -> [Require] -> [AmbVal]
eval a r = [l | x <- a, y <- r, let l = ListVal (permuteList x), (fromJustAmbVal . reqAmbVal y) l]

pattern IdentUQ i <- Var (UnQual (Ident i))
pattern (:$:) l r <- App l r

evalString :: [Amb] -> [Require] -> String -> Either String T
evalString ambs reqs str = let ast = parseExp str in
  case ast of
    (ParseFailed _ _) -> Left "unexpected input"
    _                 -> let parse = fromParseResult ast in
      case parse of
        (IdentUQ "amb" :$: IdentUQ t)
          :$: (List a)                    -> Right $ AmbT $ (amb (Just t) (List a)) : ambs
        (IdentUQ "amb")
          :$: (List a)                    -> Right $ AmbT $ (amb Nothing (List a)) : ambs
        (IdentUQ "amb" :$: IdentUQ t)
          :$: _                           -> Left "unexpected input"
        (IdentUQ "amb")
          :$: _                           -> Left "unexpected input"
        (IdentUQ "require" :$: IdentUQ t)
          :$: (Paren (Lambda r1 r2 r3))   -> Right $ ReqT $ (require (Just t) (Lambda r1 r2 r3)) : reqs
        (IdentUQ "require")
          :$: (Paren (Lambda r1 r2 r3))   -> Right $ ReqT $ (require Nothing (Lambda r1 r2 r3)) : reqs
        (IdentUQ "require" :$: IdentUQ t)
          :$: _                           -> Left "unexpected input"
        (IdentUQ "require")
          :$: _                           -> Left "unexpected input"
        IdentUQ "eval"                    -> Right $ EvalT $ eval ambs reqs
        (IdentUQ "eval") :$: _            -> Left "unexpected input" 
        _                                 -> Left "unexpected input"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

repl :: [Amb] -> [Require] -> IO ()
repl oldAmbs oldReqs = do
  input   <- readPrompt "AmbEval>>> "
  if input == "quit"
     then return ()
     else let result = evalString oldAmbs oldReqs input in
            case result of 
              Right (AmbT newAmbs) -> print newAmbs >> repl newAmbs oldReqs 
              Right (ReqT newReqs) -> print newReqs >> repl oldAmbs newReqs
              Right (EvalT x)      -> print x       >> repl oldAmbs oldReqs
              Left x               -> print x       >> repl oldAmbs oldReqs

main :: IO ()
main = repl [] []
