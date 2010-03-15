%if False

> {-# LANGUAGE GADTs            #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Basil.Database.Relational.Takusen where
> 
> import Basil.Database.Relational.Core
> import Database.Enumerator
> import Database.Sqlite.Enumerator

%endif

We can convert a value of |Base| to an sql value:

> toSql :: DBBind a Session stmt bo =>  Base a -> a -> BindA Session stmt bo
> toSql String = bindP
> toSql Int    = bindP
> toSql Bool   = bindP
