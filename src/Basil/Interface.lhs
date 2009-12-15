%if False

> {-# LANGUAGE KindSignatures   #-}

A common interface for persistency layers

> module Basil.Interface where
>
> import Basil.Core
> import Basil.References
> import Basil.Relations.InitialValues
> import Basil.Relations.PList
> import Basil.Data.TList
> import Basil.Data.TList4
> import Generics.MultiRec.Base (El)

%endif

The typeclass |Persistent| has methods for all the operations on entities.

> class Persistent (p :: (* -> *) -> * -> * -> * -> *) where
>   runIO  ::  p phi env rels a -> IO a
>   find   ::  (El phi entity) => Ref phi entity -> p phi env rels (Maybe entity)
>   new    ::  (El phi entity) 
>          =>  entity 
>          ->  PList phi entity (InitialValues phi entity rels rels) rels 
>          ->  p phi env rels (Ref phi entity)

\todo{Show how two instances can be combined}

\todo{convert to real code}

\begin{spec}
data Combined a b = (a, b, taintedList)
\end{spec}

Sketch for find/create/update algorithms

\begin{spec}
find = do  x <- find inMemoryDatabase
           case x of
             Just y   -> return (Just y)
             Nothing  -> do  y <- find relationalDB
                             case y of
                               Nothing -> return Nothing
                               Just z  -> do  add z to inMemoryDB
                                              return (Just z)

create = do  ref <- add x to inMemoryDatabase
             add ref to tainted list

update = do  update x in inMemoryDatabase
             add ref to tainted list

-- store in-memory to rdbms
persist = mapM saveItem tainted
 where saveItem = ...
                            
\end{spec}
