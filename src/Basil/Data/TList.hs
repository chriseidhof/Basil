{-# LANGUAGE KindSignatures,
             TypeFamilies,
             UndecidableInstances,
             GADTs
 #-}
module Basil.Data.TList where

import Basil.Data.TBoolean

type family TList (f :: * -> *)  (phi :: * -> *) env :: *
type instance TList f phi () = ()
type instance TList f phi (x, xs) = (f x, TList f phi xs)

lookupTList :: TIndex phi ix env
                 -> TList f phi env
                 -> f ix
lookupTList Zero = fst
lookupTList (Suc x) = lookupTList x . snd

modTList :: (f ix -> f ix)
            -> TIndex phi ix env
            -> TList f phi env
            -> TList f phi env
modTList f Zero (a,b) = (f a, b)
modTList f (Suc x) (a,b) = (a, modTList f x b)

data TIndex (phi :: * -> *) ix env where
  Zero :: TIndex phi ix (ix, env)
  Suc  :: TIndex phi ix env' -> TIndex phi ix (b, env')

type family   FilterIfTypeEq x xs :: *
type instance FilterIfTypeEq x () = ()
type instance FilterIfTypeEq x (f y z, ys) = AppendIfTrue (TypeEq x y) (f y z) (FilterIfTypeEq' x (f y z, ys))

type family   FilterIfTypeEq' x xs :: *
type instance FilterIfTypeEq' x () = ()
type instance FilterIfTypeEq' x (f y z, ys) = AppendIfTrue (TypeEq x z) (f y z) (FilterIfTypeEq x ys)

type family   AppendIfTrue bool x xs :: *
type instance AppendIfTrue True x xs  = (x, xs)
type instance AppendIfTrue False x xs = xs

