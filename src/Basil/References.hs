{-# LANGUAGE GADTs, KindSignatures #-}
module Basil.References where

data Ref     (phi :: * -> *) (ix :: *) where
  Ref { pr :: (phi ix), pKey :: Ident } ::  Ref phi ix
data RefList (phi :: * -> *) (ix :: *) where
  RLNil :: RefList phi ix
  RLCons :: Ref phi ix -> RefList phi ix -> RefList phi ix

data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type UID   = Int
