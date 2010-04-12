{-# LANGUAGE GADTs, TemplateHaskell          #-}
module Basil.InMemory.Cache (TypeCache, Cache, emptyState, cached) where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label (mkLabels, label)
import qualified Data.Map as M
import qualified Data.Set as S

newtype TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show

type Cache model = HList (TMap TypeCache model)

-- type could be "Witness es es -> Cache es", but that loops the type checker.
emptyState :: Witnesses es env -> Cache env
emptyState WNil         = Nil
emptyState (WCons _ xs) = TypeCache M.empty .*. emptyState xs

$(mkLabels [''TypeCache])
