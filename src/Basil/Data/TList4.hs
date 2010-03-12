{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- Unfortunately, this is needed for the nested type family application
{-# LANGUAGE UndecidableInstances #-}

module Basil.Data.TList4 where

import Basil.Data.TBoolean
import Basil.Data.TList

-- Typed container list.
  --


-- data TList4 (f :: * -> * -> * -> * -> * -> *) (phi :: *) a where
--   TNil4 :: TList4 f phi Nil
--   TCons4 :: Ix phi x -> Ix phi y -> f phi m1 x m2 y -> TList4 f phi xs -> TList4 f phi (f phi m1 x m2 y :*: xs)
-- 
-- type family   Filter4IfTypeEq x xs :: *
-- type instance Filter4IfTypeEq x () = ()
-- type instance Filter4IfTypeEq x (f y c1 z, ys) = AppendIfTrue (TypeEq x y) (f y c1 z) (Filter4IfTypeEq' x (f y c1 z, ys))
-- 
-- type family   Filter4IfTypeEq' x xs :: *
-- type instance Filter4IfTypeEq' x () = ()
-- type instance Filter4IfTypeEq' x (f y c1 z, ys) = AppendIfTrue (TypeEq x z) (f y c1 z) (Filter4IfTypeEq x ys)
-- 
-- lookupTList4 :: Ix phi ix -> TList4 f phi env -> ix
-- lookupTList4 Zero    (TCons4 _ _ x xs) = x
-- lookupTList4 (Suc x) (TCons4 _ _ y ys) = lookupTList4 x ys
-- 
-- -- Find relations in both ways (TODO: explain a bit more)
-- filterByType :: TEq phi => phi x -> TList4 f phi xs -> TList4 f phi (Filter4IfTypeEq x xs)
-- filterByType x TNil4 = TNil4
-- filterByType x ls@(TCons4 pr1 pr2 y ys) = case tEq x pr1 of
--                                  TTrue  -> TCons4 pr1 pr2 y (filterByType' x ls)
--                                  TFalse -> filterByType' x ls -- x ys
-- 
-- filterByType' :: TEq phi => phi x -> TList4 f phi xs -> TList4 f phi (Filter4IfTypeEq' x xs)
-- filterByType' x TNil4 = TNil4
-- filterByType' x (TCons4 pr1 pr2 y ys) = case tEq x pr2 of
--                                  TTrue  -> TCons4 pr1 pr2 y (filterByType x ys)
--                                  TFalse -> filterByType x ys -- x ys
