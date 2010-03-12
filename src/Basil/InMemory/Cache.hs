{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Basil.InMemory.Cache (TypeCache, Cache, emptyState, cached) where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base hiding (index)
import qualified Data.Map as M
import qualified Data.Set as S

data TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show

type Cache phi = HList (TMap TypeCache phi)

emptyState :: Witnesses phi -> Cache phi
emptyState WNil         = Nil
emptyState (WCons _ xs) = TypeCache M.empty .*. emptyState xs

$(mkLabels [''TypeCache])
