{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE FlexibleContexts   #-}

module Basil.Core where

import Basil.Data.TList4
import Basil.Data.TBoolean
import Basil.Data.TList
import Generics.MultiRec.Base hiding (index)

data One
data Many

data Cardinality a where
  One  :: Cardinality One
  Many :: Cardinality Many

class Fam phi => ERModel (phi :: * -> *) rels | phi -> rels, rels -> phi where
  relations :: TList4 Rel phi rels

relationsForType :: (ERModel phi xs, TEq phi) => phi x -> TList4 Rel phi (Filter4IfTypeEq x xs)
relationsForType ix = filterByType ix relations

data Rel phi cardinalityL cardinalityR l r where
  Rel  :: Cardinality cardinalityL 
       -> phi l
       -> String
       -> Cardinality cardinalityR
       -> phi r
       -> String
       -> Rel phi cardinalityL l cardinalityR r

mkRelation (nameL, cardL, entityTypeL) (nameR, cardR, entityTypeR) = Rel cardL entityTypeL nameL cardR entityTypeR nameR


