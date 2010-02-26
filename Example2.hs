{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}
--  OPTIONS_GHC -Wall

module CoreData2 where

import Basil
import Basil.Relations.PList
import Basil.Query
import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.TH
import Prelude hiding (log)
-- import qualified Basil.InMemory.Relations.Storage as B
-- import qualified Basil.InMemory.Relations as X
import qualified Basil.Relations.InitialValues as I
import qualified Data.Map as M

import Data.HList

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC {tag :: String} deriving Show

type BlogEntities = User :*: Post :*: Comment :*: Tag :*: HNil

ixUser    = hZero
ixPost    = hSucc hZero
ixComment = hSucc (hSucc hZero)
ixTag     = hSucc (hSucc (hSucc hZero))

$(mkLabels [''User, ''Post, ''Comment, ''Tag])
$(deriveConstructors [''User, ''Post, ''Comment,  ''Tag])

type BlogRelations =   (One `To` Many) User Post
                  :*:  (One `To` Many) User Comment
                  :*:  (One `To` Many) Post Comment
                  :*:  (One `To` Many) User User
                  :*:  HNil


authorPosts    = mkRelation ("author", One, ixUser)   ("posts"   , Many, ixPost)
authorComments = mkRelation ("author", One, ixUser)   ("comments", Many, ixComment)
postComments   = mkRelation ("post"  , One, ixPost)   ("comments", Many, ixComment)
parentChildren = mkRelation ("parent", One, ixUser)   ("children", Many, ixUser)

instance ERModel BlogEntities BlogRelations where
  relations = authorPosts .*.  authorComments .*.  postComments .*.  parentChildren .*. hNil


type To m1 m2 t1 t2 = Rel BlogEntities m1 t1 m2 t2


exampleUser  n = UserC    n "test" 24
examplePost    = PostC    "fipo" "my first post"
exampleComment = CommentC "a comment!"
 
--parent   x = (x, DR,  Suc (Suc (Suc Zero)))
--children x = (x, DL,  Suc (Suc (Suc Zero)))
-- 
-- authorP x = (x, DR, Zero)
-- 
-- age_ = Attribute "age" age
-- 
-- -- example flow
-- --
-- 
-- example :: Basil Blog BlogEnum BlogRelationsEnum [(Ref Blog User, User)]
-- example = do chris    <- new (exampleUser "chris") ((parent (Ref User (UID 999))) `PCons` (PNil))
--              piet     <- new (exampleUser "piet") ((parent chris) `PCons` (PNil))
--              post     <- new examplePost (PCons (authorP chris) PNil)
--              --auth     <- getRelation post (DR, Zero)
--              age      <- attr chris lAge
--              drinking <- query (Not (age_ .<. Constant 18))
--              return drinking
-- 
-- -- to run
-- runIt = runBasil $ example
-- 
-- -- Boilerplate code (will be TH eventually)
-- 
-- instance EnumTypes Blog BlogEnum where
--   allTypes      = WCons (WCons (WCons (WCons WNil)))
--   index User    = Zero
--   index Post    = Suc Zero
--   index Comment = Suc (Suc Zero)
--   index Tag     = Suc (Suc (Suc Zero))
-- 
-- -- TODO: this should be TH as well.
-- type instance TypeEq User     User    = True 
-- type instance TypeEq User     Post    = False
-- type instance TypeEq User     Comment = False
-- type instance TypeEq User     Tag     = False
-- type instance TypeEq Post     User    = False
-- type instance TypeEq Post     Post    = True 
-- type instance TypeEq Post     Comment = False
-- type instance TypeEq Post     Tag     = False
-- type instance TypeEq Comment  User    = False
-- type instance TypeEq Comment  Post    = False
-- type instance TypeEq Comment  Comment = True 
-- type instance TypeEq Comment  Tag     = False
-- type instance TypeEq Tag      User    = False
-- type instance TypeEq Tag      Post    = False
-- type instance TypeEq Tag      Comment = False
-- type instance TypeEq Tag      Tag     = True
-- 
-- -- TODO: this should be TH as well.
-- instance TEq Blog where
--  tEq User     User    = TTrue 
--  tEq User     Post    = TFalse
--  tEq User     Comment = TFalse
--  tEq User     Tag     = TFalse
--  tEq Post     User    = TFalse
--  tEq Post     Post    = TTrue 
--  tEq Post     Comment = TFalse
--  tEq Post     Tag     = TFalse
--  tEq Comment  User    = TFalse
--  tEq Comment  Post    = TFalse
--  tEq Comment  Comment = TTrue 
--  tEq Comment  Tag     = TFalse
--  tEq Tag      User    = TFalse
--  tEq Tag      Post    = TFalse
--  tEq Tag      Comment = TFalse
--  tEq Tag      Tag     = TTrue
-- 
-- 
-- 
-- -- testing
-- test = X.setValue (Ref User (Fresh 1)) ((Ref Post (Fresh 2)), DL, Zero)
--      $ test'
-- test' = 
--        X.setValue (Ref User (Fresh 1)) ((Ref Post (Fresh 3)), DL, Zero)
--      $ B.empty (relations :: TList4 Rel Blog BlogRelationsEnum)
