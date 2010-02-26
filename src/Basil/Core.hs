{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances          #-}

module Basil.Core where

import Basil.Data.TList4
import Basil.Data.TBoolean
import Basil.Data.TList
import Data.HList

data One
data Many

data Cardinality a where
  One  :: Cardinality One
  Many :: Cardinality Many

class (HList relations, EntityRefs entities relations) => ERModel entities relations | entities -> relations where
  relations :: (HList relations, EntityRefs entities relations) => relations

class    EntityRefs entities rels
instance EntityRefs entities HNil
instance (EntityRefs entities xs) => EntityRefs entities (HCons (Rel entities cl cR l r) xs)

data Rel entities cardinalityL cardinalityR l r where
  Rel  :: (  HLookupByHNat ixL entities l
           , HLookupByHNat ixR entities r
          )
       => Cardinality cardinalityL 
       -> ixL
       -> String
       -> Cardinality cardinalityR
       -> ixR
       -> String
       -> Rel entities cardinalityL l cardinalityR r

mkRelation (nameL, cardL, entityTypeL) (nameR, cardR, entityTypeR) = Rel cardL entityTypeL nameL cardR entityTypeR nameR


