{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Basil.Core where

import Basil.Data.TList4
import Basil.Data.TBoolean
import Basil.Data.TList

data One
data Many

data Cardinality a where
  One   :: Cardinality One
  Many  :: Cardinality Many

class ERModel phi rels | phi -> rels, rels -> phi where
  relations :: TList4 Rel rels
  witnesses :: Witnesses phi phi

-- relationsForType :: (ERModel phi xs, TEq phi) => phi x -> TList4 Rel phi (Filter4IfTypeEq x xs)
-- relationsForType ix = filterByType ix relations



data Rel phi cardinalityL cardinalityR l r where
  Rel  :: Cardinality cardinalityL 
       -> Ix phi l
       -> String
       -> Cardinality cardinalityR
       -> Ix phi r
       -> String
       -> Rel phi cardinalityL l cardinalityR r

mkRelation (nameL, cardL, entityTypeL) (nameR, cardR, entityTypeR) = Rel cardL entityTypeL nameL cardR entityTypeR nameR


