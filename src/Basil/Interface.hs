{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE UndecidableInstances   #-}
module Basil.Interface (runBasil, find, new, attr, getRelation, setRelation, Basil (), BasilState) where

import Basil.Core
import Basil.Cache
import Basil.Relations
import Basil.References
import Basil.Data.TBoolean
import Basil.Data.TList (TIndex, modTList, lookupTList)
import Basil.Data.TList4 (TList4)
import Generics.MultiRec.Base hiding (index)
import qualified Control.Monad.State as ST
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Record.Label hiding (set)
import Prelude hiding (mod)

type Basil phi env rels p a = (Persist p phi, EnumTypes phi env, ERModel phi rels) => ST.StateT (BasilState phi env rels) (p phi) a

data BasilState phi env rels where
  BasilState :: (EnumTypes phi env, ERModel phi rels)
             => Cache phi env 
             -> RelCache phi rels
             -> Int 
             -> BasilState phi env rels

instance (Show (RelCache phi rels), Show (Cache phi env)) => Show (BasilState phi env rels) where
  show (BasilState x y z) = "BasilState {" ++ unwords [show x, show y, show z] ++ "}"


runBasil :: forall phi p env rels a . (Persist p phi, EnumTypes phi env, ERModel phi rels) => Basil phi env rels p a -> p phi (a, BasilState phi env rels)
runBasil comp = ST.runStateT comp (BasilState (emptyState (allTypes :: Witnesses phi env)) (emptyRels (relations :: TList4 To phi rels)) 0)

find :: (Persist p phi, El phi ix) => Int -> Basil phi env rels p (Ref phi ix)
find ix = undefined -- todo return (Ref proof ix)

findCache :: (Persist p phi, El phi ix) => Ref phi ix -> Basil phi env rels p (Maybe ix)
findCache (Ref tix ix) = do st <- getM cache
                            return (M.lookup ix $ get cached $ lookupTList (index tix) st)

attr :: (Persist p phi, El phi ix) => Ref phi ix -> (ix :-> att) -> Basil phi env rels p att
attr r@(Ref tix ix) at = do val <- findCache r
                            case val of
                                 Just x  -> return $ get at x
                                 Nothing -> error "Not found in cache."

new :: (Persist p phi, El phi ix, ERModel phi rels, TEq phi) 
    => ix -> PList phi ix (InitialValues phi ix rels rels) rels -> Basil phi env rels p (Ref phi ix)
new i rels = do let tix = proof
                freshId <- getM freshVariable
                modM freshVariable (+1)
                let ident        = Fresh freshId
                    ref          = Ref tix ident
                    saveData     = mod cached  (M.insert ident i)
                    addToTainted = mod tainted (S.insert ident)
                modM cache    (modTList (saveData . addToTainted) (index tix))
                modM relCache (storeAll ref rels)
                return ref

setRelation :: (TEq phi, ERModel phi rels, Persist p phi) 
              => Ref phi r -> ValueWithPointer phi r dir (To phi m1 m2 i1 i2) rels
     -> Basil phi env rels p ()
setRelation ref rel = modM relCache (setValue ref rel)

getRelation :: (TEq phi, ERModel phi rels, Persist p phi) 
              => Ref phi r 
              -> (Dir dir, TIndex phi (To phi m1 m2 i1 i2) rels) 
              -> Basil phi env rels p (Maybe (Value dir (To phi m1 m2 i1 i2)))
getRelation ref rel = fmap (getValue ref rel) $ getM relCache


-- State helper functions
cache :: (EnumTypes phi env) => (BasilState phi env rels :-> Cache phi env)
cache = label getCache setCache
 where getCache :: (EnumTypes phi env) => BasilState phi env rels -> Cache phi env
       getCache (BasilState x _ _) = x
       setCache :: (EnumTypes phi env) => Cache phi env -> BasilState phi env rels -> BasilState phi env rels
       setCache x (BasilState _ y z) = BasilState x y z
freshVariable ::(EnumTypes phi env) => BasilState phi env rels :-> Int
freshVariable = label get' set'
 where get' :: BasilState phi env rels -> Int
       get' (BasilState _ _ x) = x
       set' :: (EnumTypes phi env) => Int -> BasilState phi env rels -> BasilState phi env rels
       set' z (BasilState x y _) = BasilState x y z

relCache ::(ERModel phi rels) => BasilState phi env rels :-> RelCache phi rels
relCache = label get' set'
 where get' :: (ERModel phi rels) => BasilState phi env rels -> RelCache phi rels
       get' (BasilState _ x _) = x
       set' :: (ERModel phi rels) => RelCache phi rels -> BasilState phi env rels -> BasilState phi env rels
       set' y (BasilState x _ z) = BasilState x y z
