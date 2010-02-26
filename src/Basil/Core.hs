{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Basil.Core where

import Basil.Data.TList4
import Basil.Data.TBoolean
import Basil.Data.TList

data One
data Many

data Cardinality a where
  One  :: Cardinality One
  Many :: Cardinality Many

class ERModel entities rels | entities -> rels, rels -> entities where
  relations :: TList4 Rel entities rels

data Rel entities cardinalityL cardinalityR l r where
  Rel  :: Cardinality cardinalityL 
       -> phi l
       -> String
       -> Cardinality cardinalityR
       -> entities r
       -> String
       -> Rel entities cardinalityL l cardinalityR r

mkRelation (nameL, cardL, entityTypeL) (nameR, cardR, entityTypeR) = Rel cardL entityTypeL nameL cardR entityTypeR nameR


