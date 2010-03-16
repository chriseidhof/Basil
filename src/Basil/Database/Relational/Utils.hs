{-# LANGUAGE FlexibleContexts #-}

module Basil.Database.Relational.Utils where

import Basil.Database.Relational.Core
import Basil.Database.Relational.Takusen
import Database.Enumerator
import Database.Sqlite.Enumerator
import Basil.Data.TList
import Data.List (intercalate)

tableSqlFields :: HList2 Attr x -> [String]
tableSqlFields = foldTList ((:) . createAttribute) []
 where createAttribute :: Attr a -> String
       createAttribute (Attr nm _ ) = nm

tableSqlPlaceholders :: HList2 Attr x -> [String]
tableSqlPlaceholders = foldTList ((:) . const createPlaceHolder) []
 where createPlaceHolder :: String
       createPlaceHolder = "?"

tableSqlValues :: HList2 (WithMeta Attr) row -> [BindA Session PreparedStmtObj BindObj]
tableSqlValues = foldTList ((:) . createValue) []
 where createValue :: WithMeta (Attr) a -> BindA Session PreparedStmtObj BindObj
       createValue (Combined a (Attr _ typ)) = toSql typ a

parens x = "(" ++ x ++ ")"
commaList = intercalate ","
