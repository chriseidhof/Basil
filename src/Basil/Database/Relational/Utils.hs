{-# LANGUAGE FlexibleContexts #-}

module Basil.Database.Relational.Utils where

import Basil.Database.Relational.Core
import Basil.Data.TList
import Data.List (intercalate)
import Data.Char (toLower)
import Basil.Database.Relational.HDBC
import Database.HDBC

tableSqlFields :: HList2 (Attr env) x -> [String]
tableSqlFields = foldTList ((:) . createAttribute) []
 where createAttribute :: Attr env a -> String
       createAttribute (Attr nm _ ) = nm
       createAttribute _            = error "createAttribute."

tableSqlPlaceholders :: HList2 (Attr env) x -> [String]
tableSqlPlaceholders = foldTList ((:) . const createPlaceHolder) []
 where createPlaceHolder :: String
       createPlaceHolder = "?"

tableSqlValues :: HList2 (WithMeta (Attr env)) row -> [SqlValue]
tableSqlValues = foldTList ((:) . createValue) []
 where createValue :: WithMeta (Attr env) a -> SqlValue
       createValue (Combined a (Attr _ typ)) = toHDBC typ a
       createValue _                         = error "createValue"

parens :: String -> String
parens x = "(" ++ x ++ ")"

commaList :: [String] -> String
commaList = intercalate ","

lower :: String -> String
lower = map toLower
