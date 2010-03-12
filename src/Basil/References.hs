{-# LANGUAGE GADTs, KindSignatures #-}
module Basil.References where

import Basil.Data.TList (Ix)

data Ref (phi :: *) (ix :: *) where
  Ref { pr :: Ix phi ix, pKey :: Ident } ::  Ref phi ix

data RefList phi ix where
  RLNil :: RefList phi ix
  RLCons :: Ref phi ix -> RefList phi ix -> RefList phi ix

instance Show (Ref phi ix) where
  show x = "Ref "  ++ show (pKey x)

instance Eq (Ref phi ix) where
  r1 == r2 = (pKey r1 == pKey r2)

instance Ord (Ref phi ix) where
  compare r1 r2 = compare (pKey r1) (pKey r2)

data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type UID   = Int
