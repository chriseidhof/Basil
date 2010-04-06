> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, TemplateHaskell, EmptyDataDecls, TypeFamilies, TypeSynonymInstances #-}

> module Basil.Database.Relational.EntitiesExample where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities
>
> import Generics.Regular
> import Basil.Data.TList hiding ((:*:))
> import qualified Basil.Data.TList as T

Suppose we have a datatype |Release| (note that this version is slightly changed from the previous examples):

> data Release	= Release	{ version :: Int , date :: Date , notes :: String }

The |Date| type is a newtype wrapper around a |String|:

> newtype Date = Date {unDate :: String}

An example value of |Release| is the |ghc612| value:

> ghc612 :: Release
> ghc612 = Release 612 (Date "11 Oct 2009") ""

We can derive the regular instances using a bit of Template Haskell that is provided by the regular library:

> $(deriveAll ''Release "PFRelease")
> type instance PF Release = PFRelease

If we try to derive the schema using |schema' ghc612|, we get the error that |Date| is not an instance of |Representable|. This means that we have to provide a way to convert a |Date| into a value that can be understood by the RDBMS. We give an instance for |Representable|:

> instance Representable Date String where
>   baseCode = const String
>   toRep    = unDate
>   fromRep  = Date

Now we can construct the schema for |Release| entities. We use the |ghc612| as input, but we could have provided any non-bottom value.

> releaseSchema = schema' ghc612

If we evaluate |releaseSchema|, we get the following result:

\begin{spec}
Cons2 (Attr version <Int>) (Cons2 (Attr date <String>) (Cons2 (Attr notes <String>) Nil2))
\end{spec}
