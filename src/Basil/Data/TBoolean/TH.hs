module Basil.Data.TBoolean.TH where

import Control.Monad
import Language.Haskell.TH.Syntax

mkEqualities :: [Name] -> Q [Dec]
mkEqualities names = liftM concat $ mapM (equality names) names

equality :: [Name] -> Name -> Q [Dec]
equality names nm = mapM (isEqual nm) names

isEqual :: Name -> Name -> Q Dec
isEqual nm1 nm2 = do
  let result = ConT $ mkName (if nm1 == nm2 then "True" else "False")
  return (TySynInstD (mkName "TypeEq") [ConT nm1, ConT nm2] result)
