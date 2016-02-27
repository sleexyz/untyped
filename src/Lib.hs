{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Lib where

import           Control.Exception   (assert)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Debug.Trace
import           System.Console.ANSI as AN
import qualified Text.Show.Pretty    as Pr


color :: Color -> String -> String
color c str = AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] ++  str ++ AN.setSGRCode [AN.Reset]


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

sym :: String -> Value
sym str = Prim str $ PrimSym str


data Context = Context { boundMap :: Map.Map Name Value
                       , freeMap  :: Set.Set Name
                       } deriving (Show)

data Prog = Prog Value Context
instance Show Prog where
  show (Prog value context) = color AN.Green (Pr.ppShow value) ++ "\n" ++ color AN.Black (Pr.ppShow context) ++ "\n"

ctxLookup :: Context -> Name -> Maybe Value
ctxLookup (Context boundMap _) name = Map.lookup name boundMap

insertBound :: Name -> Value -> Context -> Context
insertBound name expr (Context boundMap freeMap) = Context boundMap' freeMap'
  where
    boundMap' = Map.insert name expr boundMap
    freeMap' = Set.delete name freeMap

insertFree :: Name -> Context -> Context
insertFree name (Context boundMap freeMap) = Context boundMap' freeMap'
  where
    boundMap' = assert notfound boundMap
      where
        notfound = isNothing $ Map.lookup name boundMap
    freeMap' = Set.insert name freeMap

deleteBound :: Name -> Context -> Context
deleteBound name (Context boundMap freeMap) = Context boundMap' freeMap
  where
    boundMap' = Map.delete name boundMap


-- |Interpretation

-- TODO: make monadic for introspection
defaultContext = Context (Map.fromList
                          [
                            ("I", Lambda "x" (Var "x"))
                          , ("K", Lambda "x" (Lambda "y" (Var "x")))
                          -- , ("S", Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Var "x") (Var "y")) (Apply (Var "x") (Var "z"))))))
                          , ("Omega", omega)
                          -- , ("Y", ycomb)
                            -- , ("KIOmega", Apply (omega)
                          ]) (Set.fromList [])
  where
    omega = Apply om om
      where
        om = Lambda "x" (Apply (Var "x") (Var "x"))
    ycomb = Lambda "y" (Apply y' y')
      where
        y' = Lambda "x" (Apply (Var "y") (Apply (Var "x") (Var "x")))

evalStrict :: Value -> Prog
evalStrict input = eval $ Prog input defaultContext


eval :: Prog -> Prog
eval (Prog value ctx) = case value of
  -- Do Nothing
  Lambda name expr  -> Prog (Lambda name expr) ctx
  Prim name expr    -> Prog (Prim name expr)   ctx
  --  Step
  Var name          -> evalVar name
  Apply f x         -> evalApply f x

  where
    evalVar :: Name -> Prog
    evalVar name = f $ ctxLookup ctx name
      where
        f (Just expr) = eval (Prog expr ctx)
        f Nothing     = error $ "Error! " ++ name ++ " not found!"
        -- f Nothing     = Prog (Prim name (PrimSym name)) $ insertFree name ctx

    evalApply :: Value -> Value -> Prog
    evalApply bound binder = f (eval (Prog bound ctx)) (eval (Prog binder ctx)) -- Strict semantics, since we require that right side be evaluated
      where
        f (Prog (Lambda name bound') ctx') (Prog binder' _) = cleanScope $ eval $ Prog bound' (insertBound name binder' ctx') -- Do something with ctx?
          where
             cleanScope (Prog result ctx'') = Prog result ctx''


-- |Should fail under Strict evaluation, but evaluate to I under Lazy evaluation
kiomega :: Value
kiomega = Apply (Apply (Var "K") (Var "I")) (Var "Omega")

-- |Should fail under both Strict and Lazy evaluation
komegai :: Value
komegai = Apply (Apply (Var "K") (Var "Omega")) (Var "I")
