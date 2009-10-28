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

import Basil.References
import Basil.Data.TList4
import Basil.Data.TBoolean
import Basil.Data.TList
import Generics.MultiRec.Base hiding (index)
import Control.Monad.Trans (lift)
import Data.Record.Label hiding (set)
import qualified Data.Map as M
import qualified Control.Monad.State as ST

data One
data Many

data Multiplicity a where
  One  :: Multiplicity One
  Many :: Multiplicity Many

class Fam phi => ERModel (phi :: * -> *) rels | phi -> rels, rels -> phi where
  relations :: TList4 To phi rels

relationsForType :: (ERModel phi xs, TEq phi) => phi x -> TList4 To phi (FilterIfTypeEq x xs)
relationsForType ix = filterByType ix relations

data Relation phi multiplicity i1 i2 where
  Rel :: String                         -- | The name
      -> Multiplicity mult1             -- | The multiplicity
      -> phi i1                         -- | The to
      -> Relation phi (mult1, mult2) i1 i2

mult :: Relation phi (mult1,mult2) i1 i2 -> Multiplicity mult1
mult (Rel _ m _) = m

typ :: Relation phi (mult1,mult2) i1 i2 -> phi i1
typ (Rel _ _ t) = t

data To phi mult1 mult2 l r where
  To :: Multiplicity mult1 
     -> Multiplicity mult2
     -> phi l
     -> phi r
     -> Relation phi (mult1, mult2) l r 
     -> Relation phi (mult2, mult1) r l
     -> To phi mult1 mult2 l r

mkRelation x y = To (mult x) (mult y) (typ x) (typ y) x y

data Witnesses (phi :: * -> *) (env :: *) where
  WNil  :: Witnesses phi ()
  WCons :: El phi ix => Witnesses phi env -> Witnesses phi (ix, env)


class (Functor (p phi), Monad (p phi), Fam phi) => Persist (p :: (* -> *) -> * -> *) (phi :: * -> *) where
  pFetch :: phi ix -> Int -> p phi (Maybe ix)
  -- pSave  :: Regular a => TRef f a fam -> Int -> a -> p fam ()
  -- pFetchHasMany :: (Regular a, Regular b) => TRef TypeCache b fam -> NamedLabel a (Many b) -> Int -> p fam [(Int, b)]
