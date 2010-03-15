{-# LANGUAGE KindSignatures,
             TypeFamilies,
             UndecidableInstances,
             GADTs,
             MultiParamTypeClasses, FunctionalDependencies
             , TypeOperators, EmptyDataDecls, Rank2Types
 #-}
module Basil.Data.TList where

import Basil.Data.TBoolean

infixr 5 :*:
infixr 5 .*.
infixr 5 .**.

data (:*:) a b
data Nil

(.*.) = Cons
(.**.) = Cons2

class El phi ix where
  proof :: Ix phi ix

data HList a where
  Nil  :: HList Nil
  Cons :: a -> HList b -> HList (a :*: b)

data HList2 f a where
  Nil2  :: HList2 f Nil
  Cons2 :: f a -> HList2 f b -> HList2 f (a :*: b)

data Ix phi ix where
  Zero :: Ix (a :*: b) a
  Suc  :: Ix xs a -> Ix (x :*: xs) a

type family   TMap (f :: * -> *) phi :: *
type instance TMap f Nil       = Nil
type instance TMap f (a :*: b) = f a :*: TMap f b

data Witnesses phi env where
  WNil  :: Witnesses phi Nil
  WCons :: Ix phi ix -> Witnesses phi env -> Witnesses phi (ix :*: env)

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

foldTList :: (forall ix . f ix -> r -> r)
          -> r
          -> HList2 f phi
          -> r
foldTList f r Nil2 = r
foldTList f r (Cons2 x xs) = f x (foldTList f r xs)

-- fold f r [] = r
-- fold f r (x:xs) = f (f r x

tMap :: (forall b. b -> f b) -> HList a -> HList2 f a
tMap f Nil         = Nil2
tMap f (Cons x xs) = Cons2 (f x) (tMap f xs)

zipHlistWithHList2 :: HList a -> HList2 f a -> HList2 (WithMeta f) a
zipHlistWithHList2 Nil Nil2 = Nil2
zipHlistWithHList2 (Cons x xs) (Cons2 y ys) = Combined x y .**. zipHlistWithHList2 xs ys

data WithMeta f a = Combined a (f a)

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
type instance AppendIfTrue True x xs  = x :*: xs
type instance AppendIfTrue False x xs = xs
-- 
-- -- TODO: following should be in a separate module.
-- class EnumTypes phi ls | phi -> ls, ls -> phi where 
--   allTypes :: Witnesses phi ls
--   index :: phi ix -> TIndex phi ix ls
-- 
