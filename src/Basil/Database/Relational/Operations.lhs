%if False

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core

%endif

We can now define some operations on our tables. The implementations are straightforward and can be found in the library accomplishing this thesis.

> create  :: Table db row -> row -> IO Int
> read    :: Table db row -> Int -> IO (Maybe row)
> update  :: Table db row -> Int -> row -> IO ()
> delete  :: Table db row -> Int -> IO ()

%if False

> create  = undefined
> read    = undefined
> update  = undefined
> delete  = undefined

%endif
