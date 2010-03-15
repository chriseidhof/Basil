{-# LANGUAGE FlexibleContexts #-}

module Basil.Database.Relational.Utils where

import Basil.Database.Relational.Core
import Basil.Database.Relational.Takusen
import Database.Enumerator
import Database.Sqlite.Enumerator
import Basil.Data.TList
import Data.List (intercalate)

tableSqlFields :: HList2 (Attr stmt bo) x -> [String]
tableSqlFields = foldTList ((:) . createAttribute) []
 where createAttribute :: Attr stmt bo a -> String
       createAttribute (Attr nm _ _) = nm

tableSqlPlaceholders :: HList2 (Attr stmt bo) x -> [String]
tableSqlPlaceholders = foldTList ((:) . const createPlaceHolder) []
 where createPlaceHolder :: String
       createPlaceHolder = "?"

tableSqlValues :: HList2 (WithMeta (Attr stmt bo)) row -> [BindA Session stmt bo]
tableSqlValues = foldTList ((:) . createValue) []
 where createValue :: WithMeta (Attr stmt bo) a -> BindA Session stmt bo
       createValue (Combined a (Attr _ _ f)) = f a

parens x = "(" ++ x ++ ")"
commaList = intercalate ","
