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

%endif

The typeclass |Persistent| has methods for all the operations on entities.

> -- class Persistent (p :: * -> * -> * -> * -> *) where
> --   runIO  ::  p entities env rels a -> IO a
> --   find   ::  (El entities entity) => Ref entities entity -> p entities env rels (Maybe entity)
> --   new    ::  (El entities entity) 
> --          =>  entity 
> --          ->  PList entities entity (InitialValues entities entity rels rels) rels 
> --          ->  p entities env rels (Ref entities entity)

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
