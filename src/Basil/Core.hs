{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Basil.Core where

import Basil.Data.TList4
import Basil.Data.TList

data One
data Many

data Cardinality a where
  One   ::  Cardinality One
  Many  ::  Cardinality Many

class ERModel entities rels | entities -> rels, rels -> entities where
  relations  ::  TList4 Rel rels
  witnesses  ::  Witnesses entities entities


data Rel entities cardinalityL cardinalityR l r where
  Rel   ::  Cardinality cardinalityL 
        ->  Ix entities l
        ->  String
        ->  Cardinality cardinalityR
        ->  Ix entities r
        ->  String
        ->  Rel entities cardinalityL l cardinalityR r

mkRelation  ::  (String, Cardinality cardinalityL, Ix entities l)
            ->  (String, Cardinality cardinalityR, Ix entities r)
            ->  Rel entities cardinalityL l cardinalityR r
mkRelation (nameL, cardL, entityTypeL) (nameR, cardR, entityTypeR) = Rel cardL entityTypeL nameL cardR entityTypeR nameR


