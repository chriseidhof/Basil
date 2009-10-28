{-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, EmptyDataDecls, GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Basil.Relations where

import Basil.Cache
import Basil.Core
import Basil.References
import Basil.Data.TList
import Basil.Data.TList4
import Basil.Data.TBoolean
import qualified Data.Map as M
import qualified Data.Set as S

data L
data R
data Dir d where
  DL :: Dir L
  DR :: Dir R

type family Value dir rel :: *
type instance Value L (To phi m1 One r to)    = Ref phi to
type instance Value R (To phi One m2 from r)  = Ref phi from
type instance Value R (To phi Many m2 from r) = RefList phi from
type instance Value L (To phi m1  Many r to)  = RefList phi to

type ValueWithPointer phi r dir rel rels = (Value dir rel, phi r, Dir dir, TIndex phi rel rels)

type family   InitialValues (phi :: * -> *) r rels finalRels :: *
type instance InitialValues phi r ()  f = ()
type instance InitialValues phi r (To phi m1 Many from to, xs) f = (InitialValues' phi r (To phi m1 Many from to, xs) f)
type instance InitialValues phi r  (To phi m1 One from to, xs) f = AppendIfTrue (TypeEq r from) (ValueWithPointer phi r L (To phi m1 One from to) f) (InitialValues' phi r (To phi m1 One from to, xs) f)


type family   InitialValues' (phi :: * -> *) r rels finalRels :: *
type instance InitialValues' phi r (To phi One m1  from to, xs) f = AppendIfTrue (TypeEq r to)  (ValueWithPointer phi r R (To phi One m1 from to) f) (InitialValues phi r xs f)

type instance InitialValues' phi r (To phi Many m1 from to, xs) f = InitialValues phi r xs f

type family RelationStorage rel :: *
type instance RelationStorage (To phi One One  r1 r2)  = M.Map Ident Ident
type instance RelationStorage (To phi One Many r1 r2)  = M.Map Ident Ident
type instance RelationStorage (To phi Many One r1 r2)  = M.Map Ident Ident
type instance RelationStorage (To phi Many Many r1 r2) = (M.Map Int (S.Set Int), M.Map Int (S.Set Int))

data RelStorage a = RelStorage { unRelStorage :: RelationStorage a}

instance Show (RelationStorage a) => Show (RelStorage a) where show = show . unRelStorage

type RelCache phi rels = TList RelStorage phi rels

emptyRels :: TList4 To phi rels -> RelCache phi rels
emptyRels relations = worker (RelStorage . makeEmpty) relations

worker :: (forall m1 m2 r1 r2 . f phi m1 m2 r1 r2 -> g (f phi m1 m2 r1 r2)) -> TList4 f phi rels -> TList g phi rels
worker f TNil4 = ()
worker f (TCons4 _ _ rel xs) = (f rel, worker f xs)


makeEmpty :: To phi m1 m2 r1 r2 -> RelationStorage (To phi m1 m2 r1 r2)
makeEmpty (To One  One  _ _ _ _) = M.empty
makeEmpty (To One  Many _ _ _ _) = M.empty
makeEmpty (To Many One  _ _ _ _) = M.empty
makeEmpty (To Many Many _ _ _ _) = (M.empty, M.empty)


data PList (phi :: * -> *) r env rels where
 PNil :: PList phi r () rels
 PCons :: ValueWithPointer phi r dir (To phi m1 m2 i1 i2) rels 
       -> PList phi r env rels 
       -> PList phi r (ValueWithPointer phi r dir (To phi m1 m2 i1 i2) rels, env) rels

storeAll :: (TEq phi, ERModel phi rels)
         => Ref phi r
         -> PList phi r env rels
         -> RelCache phi rels
         -> RelCache phi rels
storeAll ref PNil = id
storeAll ref (PCons x xs) = setValue ref x . storeAll ref xs

indexOf x m =  do ix <- M.lookupIndex x m
                  return $ snd $ M.elemAt ix m

getValue :: (TEq phi, ERModel phi rels) 
            => Ref phi r 
            -> (Dir dir, TIndex phi (To phi m1 m2 i1 i2) rels) 
            -> TList RelStorage phi rels
            -> Maybe (Value dir (To phi m1 m2 i1 i2))
getValue ref (dir, ix) cache = f ref dir (lookupTList4 ix relations) (unRelStorage $ lookupTList ix cache)
  where f :: Ref phi r -> Dir dir -> To phi m1 m2 i1 i2 -> RelationStorage (To phi m1 m2 i1 i2) -> Maybe (Value dir (To phi m1 m2 i1 i2))
        f ref DL (To One One   tix1 tix2 _ _) = fmap (Ref tix2) . M.lookup (pKey ref)
        f ref DR (To One One   tix1 tix2 _ _) = fmap (Ref tix1) . indexOf (pKey ref)
        f ref DL (To Many One  tix1 tix2 _ _) = fmap (Ref tix2) . M.lookup (pKey ref)
        f ref DR (To One  Many tix1 tix2 _ _) = fmap (Ref tix1) . M.lookup (pKey ref)
        f _ _ _ = error "getValue: Not implemented yet."

setValue
     :: (TEq phi, ERModel phi rels) => Ref phi r -> ValueWithPointer phi r dir (To phi m1 m2 i1 i2) rels
     -> RelCache phi rels
     -> RelCache phi rels
setValue ix1 (ix2, prf, d, tix) = modTList (RelStorage . f d ix1 ix2 (lookupTList4 tix relations) . unRelStorage) tix
 where f :: forall phi d r m1 m2 i1 i2 . TEq phi 
         => Dir d 
         -> Ref phi r 
         -> Value d (To phi m1 m2 i1 i2) 
         -> To phi m1 m2 i1 i2 
         -> RelationStorage (To phi m1 m2 i1 i2) 
         -> RelationStorage (To phi m1 m2 i1 i2)
       f DL ix1 ix2 (To One One tix1 tix2 _ _) = M.insert (pKey ix1) (pKey ix2)
       f DR ix1 ix2 (To One One _ tix2 _ _)    = M.insert (pKey ix2) (pKey ix2)
       f DL ix1 ix2 (To Many One tix1 _ _ _)   = M.insert (pKey ix1) (pKey ix2)
       f DR ix1 ix2 (To Many One _ tix2 _ _)   = error "TODO" -- M.alter (Just . maybe (S.singleton (pKey ix1)) (S.insert (pKey ix1))) (pKey ix2)
       f DR ix1 ix2 (To One  Many _ tix2 _ _)   = M.insert (pKey ix1) (pKey ix2)
       f _ ix1 ix2  (To Many Many _ tix2 _ _)   = error "TODO"
