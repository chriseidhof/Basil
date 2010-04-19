{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}

module CoreData2 where

import Basil hiding ((:*:), find)
import Basil.Relations.PList
import Basil.Query
import Data.Record.Label (mkLabels, label)
import Prelude hiding (log)
import Basil.Database.Relational.Interface
import Basil.Database.Relational.Entities
import Basil.Database.Relational.Core
import Basil.Database.Relational.Schema
import Generics.Regular
import qualified Basil.Data.TList as T
import qualified Basil.Relations.InitialValues as I
import qualified Data.Map as M

data User    = UserC    {name :: String, password :: String, age :: Int} deriving (Show)
data Post    = PostC    {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC     {tag :: String} deriving Show

type Blog = User T.:*: Post T.:*: Comment T.:*: Tag T.:*: Nil


$(mkLabels [''User, ''Post, ''Comment, ''Tag])
$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(deriveAll ''Comment "PFComment")
type instance PF Comment = PFComment
$(deriveAll ''Tag "PFTag")
type instance PF Tag = PFTag

-- QuasiQuoting ideas:
--   ermodel Blog | User Post Comment Tag Nil with
--     author User  <> posts    Post*
--     author User  <> comments Comment*
--     parent User  <> children User*
--     post   Post  <> comments Comment*
--     posts  Post* <> tags Tag*

exampleUser :: String -> User
exampleUser  n = UserC    n "test" 24
examplePost ::  Post
examplePost    = PostC    "fipo" "my first post"
exampleComment :: Comment
exampleComment = CommentC "a comment!"

-- example flow
--

example :: BasilDB Blog BlogRelationsEnum (Maybe User)
example = do create ixUser (exampleUser "chris")
             find ixUser 1

-- boilerplate, will be generated using quasiquoting.

parent   x = (x, DR,  Suc (Suc (Suc Zero)))

children x = (x, DL,  Suc (Suc (Suc Zero)))

authorP x = (x, DR, Zero)

age_ = Attribute "age" age


type instance TypeEq User     User    = True 
type instance TypeEq User     Post    = False
type instance TypeEq User     Comment = False
type instance TypeEq User     Tag     = False
type instance TypeEq Post     User    = False
type instance TypeEq Post     Post    = True 
type instance TypeEq Post     Comment = False
type instance TypeEq Post     Tag     = False
type instance TypeEq Comment  User    = False
type instance TypeEq Comment  Post    = False
type instance TypeEq Comment  Comment = True 
type instance TypeEq Comment  Tag     = False
type instance TypeEq Tag      User    = False
type instance TypeEq Tag      Post    = False
type instance TypeEq Tag      Comment = False
type instance TypeEq Tag      Tag     = True

ixUser    = Zero
ixPost    = Suc (Zero)
ixComment = Suc (Suc Zero)
ixTag     = Suc (Suc (Suc Zero))

type BlogRelationsEnum =      ((One `To` Many) User Post)
                       T.:*:  ((One `To` Many) User Comment)
                       T.:*:  ((One `To` Many) Post Comment)
                       T.:*:  ((One `To` Many) User User)
                       T.:*:  Nil

instance ERModel Blog BlogRelationsEnum where
  relations = TCons4 authorPosts 
            $ TCons4 authorComments 
            $ TCons4 postComments 
            $ TCons4 parentChildren
            $ TNil4
  witnesses = WCons Zero (WCons (Suc Zero) (WCons (Suc (Suc Zero)) (WCons (Suc (Suc (Suc Zero))) WNil)))


authorPosts    = mkRelation ("author", One, ixUser)   ("posts"   , Many, ixPost)
authorComments = mkRelation ("author", One, ixUser)   ("comments", Many, ixComment)
postComments   = mkRelation ("post"  , One, ixPost)   ("comments", Many, ixComment)
parentChildren = mkRelation ("parent", One, ixUser)   ("children", Many, ixUser)

type To m1 m2 t1 t2 = Rel Blog m1 t1 m2 t2
