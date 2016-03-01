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
import           Data.Monoid
import qualified Data.Set            as Set
import           Debug.Trace
import           System.Console.ANSI as AN
import qualified Text.Show.Pretty    as Pr
import           Control.Applicative


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


data Context = Context { global :: Map.Map Name Value
                       , bound  :: Map.Map Name Value
                       , free   :: Set.Set Name
                       }
instance Show Context where
  show (Context global bmap fmap) = "\n-- bound: \n" ++ Pr.ppShow bmap ++ "\n\n-- free :\n" ++ Pr.ppShow fmap

data Prog = Prog Value Context
instance Show Prog where
  show (Prog value context) = color AN.Green ("\n" ++ Pr.ppShow value) ++ color AN.Black ("\n" ++ show context) ++ "\n"

ctxLookup :: Context -> Name -> Maybe Value
ctxLookup (Context global bound _) name = Map.lookup name global <|> Map.lookup name bound

insertBound :: Name -> Value -> Context -> Context
insertBound name expr (Context global bound free) = Context global bound' free'
  where
    bound' = Map.insert name expr bound
    free' = Set.delete name free

insertFree :: Name -> Context -> Context
insertFree name (Context global bound free) = Context global bound' free'
  where
    bound' = assert notfound bound
      where
        notfound = isNothing $ Map.lookup name bound
    free' = Set.insert name free

deleteBound :: Name -> Context -> Context
deleteBound name (Context global bound free) = Context global bound' free
  where
    bound' = Map.delete name bound


-- |Interpretation

-- TODO: make monadic for introspection
defaultContext = Context (Map.fromList
                          [
                            ("I", Lambda "x" (Var "x"))
                          , ("K", Lambda "x" (Lambda "y" (Var "x")))
                          , ("S", Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Var "x") (Var "y")) (Apply (Var "x") (Var "z"))))))
                          , ("Omega", omega)
                          , ("Y", ycomb)
                          , ("KIOmega", Apply (Apply (Var "K") (Var "I")) (omega)) -- should normalize under lazy but not strict semantics
                          , ("KOmegaI", Apply (Apply (Var "K") (Var "I")) (omega)) -- should fail to normalize for both lazy and strict semantics
                          ]) (Map.fromList []) (Set.fromList [])
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
