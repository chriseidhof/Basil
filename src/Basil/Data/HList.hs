{-# LANGUAGE KindSignatures,
             TypeFamilies,
             UndecidableInstances,
             GADTs,
             MultiParamTypeClasses, FunctionalDependencies
             , TypeOperators, EmptyDataDecls, Rank2Types
             , FlexibleInstances
 #-}
module Basil.Data.HList where

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

hTail :: HList (x :*: xs) -> HList xs
hTail (Cons _ xs) = xs

-- Looking up in lists

lookupHList :: Ix phi ix -> HList phi -> ix
lookupHList Zero     (Cons y _ ) = y
lookupHList (Suc x)  (Cons _ ys) = lookupHList x ys
lookupHList _        _           = error "lookupHList: absurd case."

lookupHList2 :: Ix phi ix -> HList2 f phi -> f ix
lookupHList2 Zero     (Cons2 y _ ) = y
lookupHList2 (Suc x)  (Cons2 _ ys) = lookupHList2 x ys
lookupHList2 _        _            = error "lookupHList: absurd case."

lookupMapHList :: Ix phi ix -> HList (TMap f phi) -> f ix
lookupMapHList Zero     (Cons y _ ) = y
lookupMapHList (Suc x)  (Cons _ ys) = lookupMapHList x ys
lookupMapHList _        _           = error "lookupMapHList: absurd case."

-- Mapping lists
mapMHList2 :: Monad m => (forall a . f a -> m b) -> HList2 f phi -> m [b]
mapMHList2 _ Nil2         = return []
mapMHList2 f (Cons2 x xs) = liftM2 (:) (f x) $ mapMHList2 f xs

-- Modifying a list at the position given by the |Ix|

modHList :: (f ix -> f ix)
            -> Ix phi ix
            -> HList (TMap f phi)
            -> HList (TMap f phi)
modHList f Zero    (Cons a b) = f a .*. b
modHList f (Suc x) (Cons a b) =   a .*. modHList f x b
modHList _ _       _          = error "modHList: absurd case."

-- Folding a list

foldHList :: (forall ix . f ix -> r -> r)
          -> r
          -> HList2 f phi
          -> r
foldHList _ r  Nil2         = r
foldHList f r  (Cons2 x xs) = f x (foldHList f r xs)

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

