{-# LANGUAGE GADTs, KindSignatures, ExistentialQuantification #-}
module Basil.References where

import Data.HList

data Ref entities entity = forall ixL . (HList entities, HLookupByHNat ixL entities entity) =>
  Ref {x :: ixL, pKey :: Ident}

type RefList phi ix = [Ref phi ix]

instance Eq (Ref phi ix) where
  r1 == r2 = (pKey r1 == pKey r2)

instance Ord (Ref phi ix) where
  compare r1 r2 = compare (pKey r1) (pKey r2)

data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type UID   = Int
