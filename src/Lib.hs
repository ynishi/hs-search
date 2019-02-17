{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import           Data.List

newtype Bit =
  Bit String
  deriving (Show)

type MaxN = Int

type MinN = Int

data Match
  = Match Bit
  | Ngram MaxN
          MinN
          Bit
  | Keyword Bit
  deriving (Show)

matchToQueryStr :: Match -> String
matchToQueryStr (Match (Bit s)) = "{\"match\": \"" ++ s ++ "\"}"
matchToQueryStr (Ngram _ _ (Bit s)) =
  "{\"match\": \"{\"query\":" ++ s ++ "\",\"analyzer\":\"ngram\"}"
matchToQueryStr (Keyword (Bit s)) = "{\"term\": \"" ++ s ++ "\"}"

type Stem = Bit

data Intent =
  Intent Stem
         [Match]
  deriving (Show)

data Query a where
  Query :: Intent -> Query Intent
  AndQ :: Query a -> Query a -> Query a
  OrQ :: Query a -> Query a -> Query a
  NotQ :: Query a -> Query a
  ListQ :: [Intent] -> Query [Intent]

deriving instance Show a => Show (Query a)

eval :: Query a -> String
eval (Query (Intent _ ms)) =
  intercalate "," $ map (\m -> "{" ++ matchToQueryStr m ++ "}") ms
eval (AndQ x y) = "{\"bool\":{\"must\":[" ++ eval x ++ "," ++ eval y ++ "]}"
eval (NotQ x) = "{\"bool\": {\"must_not\":" ++ eval x ++ "}"
eval (OrQ x y) = "{\"bool\":{\"should\":[" ++ eval x ++ "," ++ eval y ++ "]}"
eval (ListQ xs) = "[" ++ intercalate ", " $ map (eval . Query) xs ++ "]"

data Coef
  = Coef Float
         [Intent]
  | InvCoef Float
            [Intent]
  deriving (Show)

data Expr a
  = Expr a
  | And (Expr a)
        (Expr a)
  | Not (Expr a)
  | Or (Expr a)
       (Expr a)
  deriving (Show)

newtype Search =
  Search [Coef]
  deriving (Show)

and :: Coef -> Coef -> Coef
and (Coef f1 i1) (Coef f2 i2)       = Coef (f1 * f2) (i1 ++ i2)
and (InvCoef f1 i1) (InvCoef f2 i2) = InvCoef (f1 * f2) (i1 ++ i2)

someFunc :: IO ()
someFunc = print c3
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

b1 = Bit "ab"

b2 = Bit "abc"

m1 = Match b1

m2 = Ngram 2 3 b2

i1 = Intent b1 [m1]

i2 = Intent b2 [m2]

q1 = Query i1

q2 = Query i2

q2' = Query (Intent (Bit "abcd") [Ngram 4 4 (Bit "abcd")])

q3 = AndQ q1 q2

q4 = OrQ (NotQ q1) (AndQ q2 q2')

c1 = Coef 1.0 [i1]

c2 = Coef 1.0 [i2]

c3 = Lib.and c1 c2

-- simple query
-- f1 = Field "field1"
-- x' = ngam x 3 4 (ngram :: String -> Integer -> Integer -> [String]
-- q = (x `match` f1) `AND` (x' `match` f1)
-- toJson q => {query: {...}}
data SimpleQuery
  = Field String
  | SearchSQ [String]
  | SearchOneSQ String
  | SearhWithSQ String
                String
                String
  | MatchSQ SimpleQuery
            SimpleQuery
  | AndSQ SimpleQuery
          SimpleQuery
  deriving (Show)

evalSq :: SimpleQuery -> String
evalSq (Field s)           = s
evalSq (SearchSQ ss)       = concat ss
evalSq (SearchOneSQ s)     = s
evalSq (SearhWithSQ s a m) = s ++ "analyzer:" ++ a ++ ", matchType:" ++ m
evalSq (MatchSQ s1 s2)     = evalSq s1 ++ " match " ++ evalSq s2
evalSq (AndSQ s1 s2)       = evalSq s1 ++ " and " ++ evalSq s2

f1 = Field "f1"

s1 = SearchOneSQ "s1"

s2 = SearhWithSQ "s2" "ngram" "text"

sq1 = s1 `MatchSQ` f1 `AndSQ` s2 `MatchSQ` f1
