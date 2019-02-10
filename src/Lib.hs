module Lib
    ( someFunc
    ) where

data Bit = Bit String deriving (Show)

type MaxN = Int
type MinN = Int

data Match = Match Bit | Ngram MaxN MinN Bit | Keyword Bit deriving (Show)

type Stem = Bit
data Intent = Intent Stem [Match] deriving (Show)

data Coef = Coef Float [Intent] | InvCoef Float [Intent] deriving (Show)

data Expr a = Expr a | And (Expr a) (Expr a) | Not (Expr a) | Or (Expr a) (Expr a) deriving(Show)

data Search = Search [Coef] deriving (Show)

and :: Coef -> Coef -> Coef
and (Coef f1 i1) (Coef f2 i2) = Coef (f1 * f2) (i1 ++ i2) 
and (InvCoef f1 i1) (InvCoef f2 i2) = InvCoef (f1 * f2) (i1 ++ i2)

someFunc :: IO ()
someFunc = do
    putStrLn $ show c3 
    where
      b1 = Bit "ab"
      b2 = Bit "abc"
      m1 = Match b1
      m2 = Ngram 2 3 b2 
      i1 = Intent b1 [m1]
      i2 = Intent b2 [m2]
      c1 = Coef 1.0 [i1]
      c2 = Coef 1.0 [i2]
      c3 = Lib.and c1 c2
