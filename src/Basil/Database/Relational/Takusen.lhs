%if False

> {-# LANGUAGE GADTs            #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Basil.Database.Relational.Takusen where
> 
> import Basil.Database.Relational.Core

%endif

We can convert a value of |Base| to an sql value:

> toSql :: Base a -> a -> BindA Session PreparedStmtObj BindObj
> toSql String = bindP
> toSql Int    = bindP
> toSql Bool   = bindP
