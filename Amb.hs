{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import System.IO
import System.Environment
import Data.List
import Data.Bool
import Control.Applicative
import Control.Monad.State
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Text.Show.Functions


data MultiList = SList [String]
               | CList [Char]
               | IList [Integer]
               | FList [Double] deriving (Eq, Show)

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

unwrapJust :: Maybe a -> a
unwrapJust m = case m of
  Just x  -> x
  Nothing -> undefined

parseList :: Exp -> MultiList
parseList l = unwrapJust (SList <$> parseSList l
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
            | ListVal MultiList deriving (Eq, Show)

deriving instance Show (Require a)
data Require a = Require { reqTag :: Maybe String, reqAmbVal :: AmbVal -> Maybe AmbVal }

parseLiteralString :: String -> Maybe AmbVal
parseLiteralString v = case v of
  a -> Just $ StringVal a
  _ -> Nothing

parseLiteralChar :: Char -> Maybe AmbVal
parseLiteralChar v = case v of
  a -> Just $ CharVal a
  _ -> Nothing

parseLiteralInt :: Integer -> Maybe AmbVal
parseLiteralInt v = case v of
  a -> Just $ IntVal a
  _ -> Nothing

intOp :: (Integer -> Integer -> Integer) -> AmbVal -> AmbVal -> Maybe AmbVal
intOp f (IntVal x) (IntVal y) = Just $ IntVal $ f x y
intOp f _ _ = Nothing

fracOp :: (Double -> Double -> Double) -> AmbVal -> AmbVal -> Maybe AmbVal
fracOp f (FloatVal x) (FloatVal y) = Just $ FloatVal $ f x y
fracOp f _ _ = Nothing

eqOp :: (forall a. Eq a => a -> a -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
eqOp f x y = Just $ BoolVal $ f x y
eqOp f _ _ = Nothing

ordOp :: (forall a. (Ord a) => a -> a -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
ordOp f (IntVal x) (IntVal y) = Just $ BoolVal $ f x y
ordOp f (FloatVal x) (FloatVal y) = Just $ BoolVal $ f x y
ordOp f _ _ = Nothing

boolOp ::(Bool -> Bool -> Bool) -> AmbVal -> AmbVal -> Maybe AmbVal
boolOp f (BoolVal x) (BoolVal y) = Just $ BoolVal $ f x y
boolOp f _ _ = Nothing

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
  _    -> Nothing

parseExpr :: String -> Exp -> Maybe (AmbVal -> Maybe AmbVal)
parseExpr var (Var (UnQual (Ident var'))) | var == var' = Just Just
parseExpr var (Lit (String i)) = Just (\_ -> parseLiteralString i)
parseExpr var (Lit (Char i))   = Just (\_ -> parseLiteralChar i)
parseExpr var (Lit (Int i))    = Just (\_ -> parseLiteralInt i)
parseExpr var (InfixApp l (QVarOp (UnQual (Symbol sym))) r) = do
  lhs <- parseExpr var l
  op <- parseOp sym
  rhs <- parseExpr var r
  return $ \val -> do
    lval <- lhs val
    rval <- rhs val
    op lval rval
parseExpr var _ = Nothing

parseLambda :: Exp -> Maybe (AmbVal -> Maybe AmbVal) 
parseLambda (Lambda _ [PVar (Ident var)] body) = parseExpr var body

parseRequire :: Maybe String -> Exp -> Require a
parseRequire tag value = Require tag $ unwrapJust $ parseLambda value

--------------------------------------------------------------------

data T = AmbT [Amb]
       | ReqT [Require AmbVal]
       | EvalT [MultiList] deriving (Show)

parseAST :: String -> Exp
parseAST s = fromParseResult $ parseExp s

amb :: Maybe String -> Exp -> Amb
amb t a = parseAmb t a

require :: Maybe String -> Exp -> Require AmbVal
require t r =  parseRequire t r

unwrapJustAmbVal :: Maybe AmbVal -> Bool
unwrapJustAmbVal m = case m of
  Just (BoolVal True)  -> True
  Just (BoolVal False) -> False
  Nothing              -> undefined

eval :: [Amb] -> [Require AmbVal] -> [MultiList]
eval [] r = []
eval a [] = concat $ permutations $ map ambVal a
eval a@(x:xs) r@(y:ys) = if ambTag x == reqTag y
                            then do filter (unwrapJustAmbVal . reqAmbVal y)
                                           (concat $ permutations $ [ListVal $ ambVal x])
                                    eval xs [y]
                                    eval [x] ys
                                 else do eval xs [y]
                                         eval [x] ys

evalString :: [Amb] -> [Require AmbVal] -> String -> Either String T
evalString ambs reqs str = let ast = parseAST str in
  case ast of
    App (App (Var (UnQual (Ident "amb")))
             (Var (UnQual (Ident x)))) (List _)         -> Right $ AmbT $ (amb (Just x) ast) : ambs
    App (Var (UnQual (Ident "amb")))   (List _)         -> Right $ AmbT $ (amb Nothing ast) : ambs
    App (App (Var (UnQual (Ident "require"))) 
             (Var (UnQual (Ident x))))   (Lambda _ _ _) -> Right $ ReqT $ (require (Just x) ast) : reqs
    App (Var (UnQual (Ident "require"))) (Lambda _ _ _) -> Right $ ReqT $ (require Nothing ast) : reqs
    Var (UnQual (Ident "eval"))                         -> Right $ EvalT $ eval ambs reqs
    _                                                   -> Left "unexpected input"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

repl :: [Amb] -> [Require AmbVal] -> IO ()
repl oldAmbs oldReqs = do
  input   <- readPrompt "AmbEval>>> "
  if input == "quit" 
     then return ()
     else let result = evalString oldAmbs oldReqs input in
            case result of 
              Right (AmbT newAmbs) -> print result >> repl newAmbs oldReqs 
              Right (ReqT newReqs) -> print result >> repl oldAmbs newReqs
              Right (EvalT _)      -> print result >> repl oldAmbs oldReqs
              Left _               -> print result >> repl oldAmbs oldReqs

main :: IO ()
main = repl [] []
