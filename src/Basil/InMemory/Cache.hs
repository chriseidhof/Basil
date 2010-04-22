{-# LANGUAGE GADTs, TemplateHaskell          #-}
module Basil.InMemory.Cache (TypeCache, Cache, emptyState, cached) where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label 
import qualified Data.Map as M
import qualified Data.Set as S

-- todo: replace by intmap

newtype TypeCache a = TypeCache {_cached :: M.Map Int a} deriving Show

type Cache model = HList (TMap TypeCache model)

emptyState :: Witnesses es env -> Cache env
emptyState WNil         = Nil
emptyState (WCons _ xs) = TypeCache M.empty .*. emptyState xs

$(mkLabels [''TypeCache])
