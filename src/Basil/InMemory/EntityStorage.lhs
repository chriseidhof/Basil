> {-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
> {-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
>
> module Basil.InMemory.EntityStorage where
> 
> import Data.HList
> import Basil.Core
> import Basil.References
> import qualified Data.Map as M
>
> data MkTypeCache = MkTypeCache
> data TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show
> instance Apply MkTypeCache x (TypeCache x) where
>   apply _ _ = TypeCache M.empty

> class EntityStorage entities storage | entities -> storage where
>   entityStorage :: entities -> storage

> instance (HMap MkTypeCache entities storage) => EntityStorage entities storage where
>   entityStorage entities = hMap MkTypeCache entities

