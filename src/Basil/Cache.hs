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
import qualified Data.Map as M
import qualified Data.Set as S

type Cache phi env = TList TypeCache phi env
data TypeCache a = TypeCache {_cached :: M.Map Ident a, _tainted :: S.Set Ident} deriving Show

emptyState :: Witnesses phi env -> Cache phi env
emptyState WNil       = ()
emptyState (WCons xs) = (TypeCache M.empty S.empty, emptyState xs)

class EnumTypes phi env | phi -> env, env -> phi where 
  allTypes :: Witnesses phi env
  index :: phi ix -> TIndex phi ix env

$(mkLabels [''TypeCache])
