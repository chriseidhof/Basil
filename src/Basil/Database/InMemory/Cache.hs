{-# LANGUAGE GADTs, TemplateHaskell          #-}
module Basil.Database.InMemory.Cache (EntityCache, Cache, emptyState, cached) where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label 
import qualified Data.IntMap as M
import qualified Data.Set as S

newtype EntityCache a = EntityCache {_cached :: M.IntMap a} deriving Show

type Cache model = HList (TMap EntityCache model)

emptyState :: Witnesses es env -> Cache env
emptyState WNil         = Nil
emptyState (WCons _ xs) = EntityCache M.empty .*. emptyState xs

$(mkLabels [''EntityCache])
