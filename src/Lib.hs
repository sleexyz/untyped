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
             | PrimSym String
             deriving (Show, Eq, Ord)

type Name = String

data Value =
    Var Name
  | Lambda Name Value
  | Prim Name PrimVal
  | Apply Value  Value
    deriving (Show, Eq)


data Context = Context { boundMap :: Map.Map Name Value
                       , freeMap :: Set.Set Name
                       } deriving (Show)

ctxLookup :: Context -> Name -> Maybe Value
ctxLookup (Context boundMap _) name = Map.lookup name boundMap

insertBound :: Name -> Value -> Context -> Context
insertBound name expr (Context boundMap freeMap) = Context boundMap' freeMap'
  where
    boundMap' = Map.insert name expr boundMap
    freeMap' = Set.delete name freeMap

insertFree :: Name -> Value -> Context -> Context
insertFree name expr (Context boundMap freeMap) = Context boundMap freeMap
  where
    insertFree (Context boundMap freeMap) name expr = Context boundMap freeMap

defaultContext :: Context
defaultContext = Context (Map.fromList
                           [
                           ]) (Set.fromList [])


-- TODO: make monadic for introspection

data Prog = Prog Value Context

eval :: Prog -> Prog
eval (Prog value ctx) = case value of

  -- Do Nothing
  Lambda name expr  -> Prog (Lambda name expr) ctx
  Prim name expr    -> Prog (Prim name expr)   ctx

  --  Step
  Var name          -> evalVar name
  -- Apply f x         -> evalApply f x
  where
    evalVar :: Name -> Prog
    evalVar name = f $ ctxLookup ctx name
      where
        f (Just expr) = undefined
        -- f Nothing     = error $ "Error! " ++ name ++ " not found!"
        f Nothing     = Prog (Prim name (PrimSym name)) ctx

-- eval :: Value -> Context -> Value
-- eval input ctx = case input of
--   Var name         -> evalVar name
--   Lambda name expr ->  Lambda name expr
--   Const prim       ->  Const prim
--   Prim name expr    ->  Prim name expr
--   Free name        ->  Free name
--   Apply f x        -> evalApply f x

--   where
--     evalVar name = f $ lookupContext ctx name
--       where
--         f (Just expr) = expr
--         -- f Nothing     = error $ "Error! " ++ name ++ " not found!"
--         f Nothing     = Free name

--     evalApply !leftExpr !rightExpr = evalFirst (eval leftExpr ctx) (eval rightExpr ctx)
--       where
--         evalFirst (Lambda name expr) right = eval (traceShowId expr) $ bindInContext ctx name right
--         evalFirst l r = Apply l r


-- -- testExpr = Apply (Lambda "x" (Var "x")) (Var "y")
-- testExpr = Apply (Lambda "x" (Var "x")) (Const $ PrimInt 2)

-- icomb = Lambda "x" (Var "x")
-- kcomb = Lambda "x" (Lambda "y" (Var "x"))
-- scomb = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Var "x") (Var "z")) (Apply (Var"y") (Var "y")))))

-- omega = Apply om om
--   where
--     om = Lambda "x" (Apply (Var "x") (Var "x"))

-- ycomb = Lambda "y" (Apply y y)
--   where
--     y = Lambda "x" (Apply (Var "y") (Apply (Var "x") (Var "x")))

-- -- kiomega = (Apply (Apply kcomb icomb) icomb)
-- -- komegai = (Apply (Apply kcomb icomb) icomb)
-- -- TODO: get KIOmega to work
-- testExpr1 = Apply icomb $ Free "hello"

-- eval' :: Value -> Value
-- eval' input = eval input defaultContext
