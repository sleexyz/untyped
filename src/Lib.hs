{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}

module Lib where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Debug.Trace

data PrimVal = PrimInt Int
             deriving (Show, Eq, Ord)

type Name = String

data Value =
    Var Name
  | Lambda Name Value
  | Prim Name Value
  | Const PrimVal
  | Apply Value  Value
  | Free Name
    deriving (Show, Eq)


data Context = Context (Map.Map Name Value) (Set.Set Name)
                deriving (Show)

lookupContext :: Context -> Name -> Maybe Value
lookupContext (Context contextMap _) name = Map.lookup name contextMap

bindInContext :: Context -> Name -> Value -> Context
bindInContext (Context contextMap undefinedSet) name expr  = Context newContextMap newUndefinedSet
  where
    newContextMap = Map.insert name expr contextMap
    newUndefinedSet = Set.delete name undefinedSet

defaultContext :: Context
defaultContext = Context (Map.fromList
                           [
                           ]) (Set.fromList [])



eval :: Value -> Context -> Value
eval input ctx = case input of
  Var name         -> evalVar name
  Lambda name expr ->  Lambda name expr
  Const prim       ->  Const prim
  Prim name expr    ->  Prim name expr
  Free name        ->  Free name
  Apply f x        -> evalApply f x

  where
    evalVar name = f $ lookupContext ctx name
      where
        f (Just expr) = expr
        -- f Nothing     = error $ "Error! " ++ name ++ " not found!"
        f Nothing     = Free name

    evalApply !leftExpr !rightExpr = evalFirst (eval leftExpr ctx) (eval rightExpr ctx)
      where
        evalFirst (Lambda name expr) right = eval (traceShowId expr) $ bindInContext ctx name right
        evalFirst l r = Apply l r


-- testExpr = Apply (Lambda "x" (Var "x")) (Var "y")
testExpr = Apply (Lambda "x" (Var "x")) (Const $ PrimInt 2)

icomb = Lambda "x" (Var "x")
kcomb = Lambda "x" (Lambda "y" (Var "x"))
scomb = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Var "x") (Var "z")) (Apply (Var"y") (Var "y")))))

omega = Apply om om
  where
    om = Lambda "x" (Apply (Var "x") (Var "x"))

ycomb = Lambda "y" (Apply y y)
  where
    y = Lambda "x" (Apply (Var "y") (Apply (Var "x") (Var "x")))

-- kiomega = (Apply (Apply kcomb icomb) icomb)
-- komegai = (Apply (Apply kcomb icomb) icomb)
-- TODO: get KIOmega to work
testExpr1 = Apply icomb $ Free "hello"

eval' :: Value -> Value
eval' input = eval input defaultContext
