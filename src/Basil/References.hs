{-# LANGUAGE GADTs, KindSignatures #-}
module Basil.References where

import Basil.Data.TList (Ix)

data Ref entities a where
  Ref :: { pr :: Ix entities a, pKey :: Ident } -> Ref entities a

data RefList entities ix where
  RLNil :: RefList entities ix
  RLCons :: Ref entities ix -> RefList entities ix -> RefList entities ix

instance Show (Ref entities ix) where
  show x = "Ref "  ++ show (pKey x)

instance Eq (Ref entities ix) where
  r1 == r2 = (pKey r1 == pKey r2)

instance Ord (Ref entities ix) where
  compare r1 r2 = compare (pKey r1) (pKey r2)

data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type UID   = Int
