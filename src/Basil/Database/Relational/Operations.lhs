%if False

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Data.TList

%endif

We can now define some operations on our tables. The implementations are straightforward and can be found in the library accomplishing this thesis.

> create  :: Table row -> HList row -> IO Int
> read    :: Table row -> Int -> IO (Maybe (HList row))
> update  :: Table row -> Int -> HList row -> IO ()
> delete  :: Table row -> Int -> IO ()

%if False

> create (nm, keys) row = unwords
>  [ "INSERT INTO "
>  , nm
>  , parens (tableSqlFields keys)
>  , "VALUES"
>  , parens (tableSqlValues row)
>  ]

> read    = undefined
> update  = undefined
> delete  = undefined

%endif
