{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Basil.InMemory.Cache (Cache, TypeCache, emptyState, cached) where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base hiding (index)
import qualified Data.Map as M
import qualified Data.Set as S

type Cache phi env = TList TypeCache phi env
data TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show

emptyState :: Witnesses phi env -> Cache phi env
emptyState WNil       = ()
emptyState (WCons xs) = (TypeCache M.empty, emptyState xs)


$(mkLabels [''TypeCache])
