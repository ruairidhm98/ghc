{-

This test is inspired by
  Practical dependent type checking using twin types
  Victor López Juan and Nils Anders Danielsson
  TyDe '20
  https://dl.acm.org/doi/10.1145/3406089.3409030
-}

{-# LANGUAGE TypeOperators, TypeApplications, DataKinds,
             StandaloneKindSignatures, PolyKinds, GADTs,
             TypeFamilies #-}

module LopezJuan where

import Data.Type.Equality ( (:~~:)(..) )
import Data.Kind ( Type )

-- amazingly, this worked without modification

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

data BoolOp where
  None :: Bool -> BoolOp
  Some :: Bool -> BoolOp

type F :: Bool -> Type
type family F b

get :: BoolOp -> Bool
get (None _) = True
get (Some x) = x

type Get :: BoolOp -> Bool
type family Get x where
  Get (None _) = True
  Get (Some x) = x

type TyFun :: Type -> Type -> Type
data TyFun arg res

type (~>) :: Type -> Type -> Type
type arg ~> res = TyFun arg res -> Type
infixr 0 ~>

type (@@) :: (a ~> b) -> a -> b
type family f @@ arg

data Const :: a -> (b ~> a)

f :: SBool x -> (:~~:) @(F (Get (_alpha x)) ~> BoolOp)
                       @(F True ~> BoolOp)
                       (Const (None x))
                       (Const (_alpha x))
f _ = HRefl
