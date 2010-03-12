{-# LANGUAGE KindSignatures,
             TypeFamilies,
             UndecidableInstances,
             GADTs,
             MultiParamTypeClasses, FunctionalDependencies
             , TypeOperators, EmptyDataDecls, Rank2Types
 #-}
module Basil.Data.TList where

import Basil.Data.TBoolean

data (:*:) a b
data Nil

(.*.) = Cons

data HList a where
  Nil  :: HList Nil
  Cons :: a -> HList b -> HList (a :*: b)

data Ix phi ix where
  Zero :: Ix (a :*: b) a
  Suc  :: Ix xs a -> Ix (x :*: xs) a

type family   TMap (f :: * -> *) phi :: *
type instance TMap f Nil       = Nil
type instance TMap f (a :*: b) = f a :*: TMap f b

data Witnesses phi where
  WNil  :: Witnesses Nil
  WCons :: Ix ix env -> Witnesses env -> Witnesses (ix :*: env)

lookupTList :: Ix phi ix -> HList phi -> ix
lookupTList Zero     (Cons y ys) = y
lookupTList (Suc x)  (Cons y ys) = lookupTList x ys

lookupMapTList :: Ix phi ix -> HList (TMap f phi) -> f ix
lookupMapTList Zero     (Cons y ys) = y
lookupMapTList (Suc x)  (Cons y ys) = lookupMapTList x ys

modTList :: (f ix -> f ix)
            -> Ix phi ix
            -> HList (TMap f phi)
            -> HList (TMap f phi)
modTList f Zero    (Cons a b) = f a .*. b
modTList f (Suc x) (Cons a b) =   a .*. modTList f x b

tMap :: (forall b. b -> f b) -> HList a -> HList (TMap f a)
tMap f Nil         = Nil
tMap f (Cons x xs) = Cons (f x) (tMap f xs)

-- type family TList (f :: * -> *)  (phi :: * -> *) env :: *
-- type instance TList f phi () = ()
-- type instance TList f phi (x, xs) = (f x, TList f phi xs)
-- 
-- infixr .*
-- (.*) = (,)
-- 
-- lookupTList :: TIndex phi ix env
--                  -> TList f phi env
--                  -> f ix
-- lookupTList Zero = fst
-- lookupTList (Suc x) = lookupTList x . snd
-- 
-- 
-- data TIndex phi ix where
--   Zero :: TIndex phi ix (ix, env)
--   Suc  :: TIndex phi ix env' -> TIndex phi ix (b, env')
-- 
-- 
-- 
-- 
-- type family   FilterIfTypeEq x xs :: *
-- type instance FilterIfTypeEq x () = ()
-- type instance FilterIfTypeEq x (f y z, ys) = AppendIfTrue (TypeEq x y) (f y z) (FilterIfTypeEq' x (f y z, ys))
-- 
-- type family   FilterIfTypeEq' x xs :: *
-- type instance FilterIfTypeEq' x () = ()
-- type instance FilterIfTypeEq' x (f y z, ys) = AppendIfTrue (TypeEq x z) (f y z) (FilterIfTypeEq x ys)
-- 
type family   AppendIfTrue bool x xs :: *
type instance AppendIfTrue True x xs  = (x, xs)
type instance AppendIfTrue False x xs = xs
-- 
-- -- TODO: following should be in a separate module.
-- class EnumTypes phi ls | phi -> ls, ls -> phi where 
--   allTypes :: Witnesses phi ls
--   index :: phi ix -> TIndex phi ix ls
-- 
