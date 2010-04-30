{-# LANGUAGE KindSignatures,
             TypeFamilies,
             UndecidableInstances,
             GADTs,
             MultiParamTypeClasses, FunctionalDependencies
             , TypeOperators, EmptyDataDecls, Rank2Types
             , FlexibleInstances
 #-}
module Basil.Data.TList where

import Basil.Data.TBoolean
import Control.Monad (liftM2)

infixr 5 :*:
infixr 5 .*.
infixr 5 .**.

-- Type for a non-empty list
data (:*:) a b

-- Type for a nil-list
data Nil

-- Handy combinators
(.*.) :: a -> HList b -> HList (a :*: b)
(.*.) = Cons

(.**.) :: f a -> HList2 f b -> HList2 f (a :*: b)
(.**.) = Cons2

-- Simple heterogenous lists
data HList a where
  Nil  :: HList Nil
  Cons :: a -> HList b -> HList (a :*: b)

-- Heterogenous lists that contain functors
data HList2 f a where
  Nil2  :: HList2 f Nil
  Cons2 :: f a -> HList2 f b -> HList2 f (a :*: b)

-- Typed indexes into the lists
data Ix phi ix where
  Zero :: Ix (a :*: b) a
  Suc  :: Ix xs a -> Ix (x :*: xs) a

-- Type-level map
type family   TMap  (f :: * -> *) phi        :: *
type instance TMap  f             Nil        = Nil
type instance TMap  f             (a :*: b)  = f a :*: TMap f b

data Witnesses finalEnv env where
  WNil  :: Witnesses finalEnv Nil
  WCons :: Ix finalEnv ix -> Witnesses finalEnv env -> Witnesses finalEnv (ix :*: env)

-- Head and tail

hHead :: HList (x :*: xs) -> x
hHead (Cons x _) = x

hTail :: HList (x :*: xs) -> xs
hTail (Cons _ xs) = xs

-- Looking up in lists

lookupTList :: Ix phi ix -> HList phi -> ix
lookupTList Zero     (Cons y _ ) = y
lookupTList (Suc x)  (Cons _ ys) = lookupTList x ys
lookupTList _        _           = error "lookupTList: absurd case."

lookupHList2 :: Ix phi ix -> HList2 f phi -> f ix
lookupHList2 Zero     (Cons2 y _ ) = y
lookupHList2 (Suc x)  (Cons2 _ ys) = lookupHList2 x ys
lookupHList2 _        _            = error "lookupTList: absurd case."

lookupMapTList :: Ix phi ix -> HList (TMap f phi) -> f ix
lookupMapTList Zero     (Cons y _ ) = y
lookupMapTList (Suc x)  (Cons _ ys) = lookupMapTList x ys
lookupMapTList _        _           = error "lookupMapTList: absurd case."

-- Mapping lists
mapMHList2 :: Monad m => (forall a . f a -> m b) -> HList2 f phi -> m [b]
mapMHList2 _ Nil2         = return []
mapMHList2 f (Cons2 x xs) = liftM2 (:) (f x) $ mapMHList2 f xs

-- Modifying a list at the position given by the |Ix|

modTList :: (f ix -> f ix)
            -> Ix phi ix
            -> HList (TMap f phi)
            -> HList (TMap f phi)
modTList f Zero    (Cons a b) = f a .*. b
modTList f (Suc x) (Cons a b) =   a .*. modTList f x b
modTList _ _       _          = error "modTList: absurd case."

-- Folding a list

foldTList :: (forall ix . f ix -> r -> r)
          -> r
          -> HList2 f phi
          -> r
foldTList _ r  Nil2         = r
foldTList f r  (Cons2 x xs) = f x (foldTList f r xs)

-- Zipping an |HList| with an |HList2|:

zipHlistWithHList2 :: HList a -> HList2 f a -> HList2 (WithMeta f) a
zipHlistWithHList2 Nil Nil2 = Nil2
zipHlistWithHList2 (Cons x xs) (Cons2 y ys) = Combined x y .**. zipHlistWithHList2 xs ys
zipHlistWithHList2 _ _ = error "zipHlistWithHList2: should not happen."

data WithMeta f a = Combined a (f a)

-- The |AppendIfTrue| type-level function

type family   AppendIfTrue  bool   x  xs   :: *
type instance AppendIfTrue  True   x  xs   = x :*: xs
type instance AppendIfTrue  False  x  xs   = xs


-- Some show instances

instance Show (HList Nil) where
   show Nil = "Nil"

instance (Show a, Show (HList as)) => Show (HList (a :*: as)) where
   show (Cons a b) = "Cons (" ++ show a ++ ") (" ++ show b ++ ")"

instance Show (HList2 f Nil) where 
   show Nil2 = "Nil2"

instance (Show (f a), Show (HList2 f b)) => Show (HList2 f (a :*: b)) where 
   show (Cons2 a b) = "Cons2 (" ++ show a ++ ") (" ++ show b ++ ")"

