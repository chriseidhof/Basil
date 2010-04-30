{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- Unfortunately, this is needed for the nested type family application
{-# LANGUAGE UndecidableInstances #-}

module Basil.Data.HList4 where

import Basil.Data.HList

-- Typed container list.

data HList4 (f :: * -> * -> * -> * -> * -> *) a where
   TNil4  :: HList4 f Nil
   TCons4 :: f phi m1 x m2 y -> HList4 f xs -> HList4 f (f phi m1 x m2 y :*: xs)

lookupHList4 :: Ix rels rel -> HList4 f rels -> rel
lookupHList4 Zero    (TCons4 x _) = x
lookupHList4 (Suc x) (TCons4 _ ys) = lookupHList4 x ys
lookupHList4 _       _             = error "lookupHList4: absurd pattern."
