{-# LANGUAGE GADTs #-}
module Basil.Database.Relational.HDBC where

import Database.HDBC
import Basil.Database.Relational.Core

toHDBC :: Base a -> a -> SqlValue
toHDBC String = toSql
toHDBC Int    = toSql
toHDBC Bool   = toSql

fromHDBC :: Attr env a -> SqlValue -> a
fromHDBC (Attr _ String) = fromSql
fromHDBC (Attr _ Int   ) = fromSql
fromHDBC (Attr _ Bool  ) = fromSql
fromHDBC (Foreign _ _ )  = ForeignKey . fromSql
