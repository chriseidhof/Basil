{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Basil.Data.TBoolean where

-- There should be an instance for all combinations of the types in your domain.
type family TypeEq a b ::  *

data TBool a where
  TTrue  :: TBool True
  TFalse :: TBool False

data True
data False

class TEq phi where
  tEq :: phi x -> phi y -> TBool (TypeEq x y)

tOr :: TBool x -> TBool y -> TBool (Or x y)
tOr = undefined

type family Or x y :: *
type instance Or True  True  = True
type instance Or True  False = True
type instance Or False True  = True
type instance Or False False = False
