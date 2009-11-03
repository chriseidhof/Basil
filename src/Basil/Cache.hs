{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Basil.Cache where

import Basil.Core
import Basil.Data.TList4
import Basil.Data.TList
import Basil.References
import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base hiding (index)
import qualified Data.Map as M
import qualified Data.Set as S

type Cache phi env = TList TypeCache phi env
data TypeCache a = TypeCache {_cached :: M.Map Ident a, _tainted :: S.Set Ident} deriving Show

emptyState :: Witnesses phi env -> Cache phi env
emptyState WNil       = ()
emptyState (WCons xs) = (TypeCache M.empty S.empty, emptyState xs)


$(mkLabels [''TypeCache])

class (Functor (p phi), Monad (p phi), Fam phi) => Persist (p :: (* -> *) -> * -> *) (phi :: * -> *) where
  pFetch :: phi ix -> Int -> p phi (Maybe ix)
  -- pSave  :: Regular a => TRef f a fam -> Int -> a -> p fam ()
  -- pFetchHasMany :: (Regular a, Regular b) => TRef TypeCache b fam -> NamedLabel a (Many b) -> Int -> p fam [(Int, b)]
