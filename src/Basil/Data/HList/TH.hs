module Basil.Data.HList.TH where

import Control.Monad
import Language.Haskell.TH.Syntax

mkTList :: String -> [Name] -> Q [Dec]
mkTList nm els = do   let name = mkName nm
                      ixs <- mkIndexes name els
                      let dec = TySynD name [] (foldr listCons (ConT $ mkName "Nil") els)
                      return (dec:ixs)
 where listCons el = AppT (AppT (ConT $ mkQName "T" ":*:") (ConT el))

mkQName :: String -> String -> Name
mkQName m n = Name (mkOccName n) (NameQ $ mkModName m)

mkIndexes :: Name -> [Name] -> Q [Dec]
mkIndexes name names = liftM concat $ mapM (mkIndex name) (zip names [0..])

mkIndex :: Name -> (Name, Int) -> Q [Dec]
mkIndex listName (elName, i) = 
  let n = mkName $ "ix" ++ nameBase elName
  in return [ SigD n $ AppT (AppT (ConT $ mkName "Ix") (ConT listName)) (ConT elName)
            , FunD n [Clause [] (NormalB (ixValue i)) []]
            ]

ixValue :: Int -> Exp
ixValue 0 = ConE (mkName "Zero")
ixValue n = AppE (ConE (mkName "Suc")) (ixValue (n-1))
