{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}

module CoreData2 where

import Basil
import Basil.Relations.PList
import Basil.Query
import Data.Record.Label (mkLabels, label)
import Prelude hiding (log)
import qualified Basil.InMemory.Relations.Storage as B
import qualified Basil.InMemory.Relations as X
import qualified Basil.Relations.InitialValues as I
import qualified Data.Map as M

data User    = UserC    {name :: String, password :: String, age :: Int} deriving (Show)
data Post    = PostC    {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC     {tag :: String} deriving Show

type Blog = User :*: Post :*: Comment :*: Tag :*: Nil


$(mkLabels [''User, ''Post, ''Comment, ''Tag])

-- QuasiQuoting ideas:
--   ermodel Blog | User Post Comment Tag Nil with
--     author User  <> posts    Post*
--     author User  <> comments Comment*
--     parent User  <> children User*
--     post   Post  <> comments Comment*
--     posts  Post* <> tags Tag*

exampleUser  n = UserC    n "test" 24
examplePost    = PostC    "fipo" "my first post"
exampleComment = CommentC "a comment!"

-- example flow
--

example :: Basil Blog BlogRelationsEnum [(Ref Blog User, User)]
example = do chris    <- new ixUser (exampleUser "chris") ((parent (Ref ixUser (UID 999))) `PCons` (PNil))
             piet     <- new ixUser (exampleUser "piet") ((parent chris) `PCons` PNil)
             post     <- new ixPost examplePost (PCons (authorP chris) PNil)
             --auth     <- getRelation post (DR, Zero)
             age      <- attr chris lAge
             drinking <- query ixUser (Not (age_ .<. Constant 18))
             return drinking

-- to run
runIt = runBasil $ example

-- testing
test = X.setValue (Ref ixUser (Fresh 1)) ((Ref ixPost (Fresh 2)), DL, Zero)
     $ test'
test' = 
       X.setValue (Ref ixUser (Fresh 1)) ((Ref ixPost (Fresh 3)), DL, Zero)
     $ B.empty (relations :: TList4 Rel BlogRelationsEnum)

-- boilerplate, will be generated using quasiquoting.

parent :: Ref Blog User -> InitialValue Blog User R ((One `To` Many) User User) BlogRelationsEnum
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

type BlogRelationsEnum =    ((One `To` Many) User Post)
                       :*:  ((One `To` Many) User Comment)
                       :*:  ((One `To` Many) Post Comment)
                       :*:  ((One `To` Many) User User)
                       :*:  Nil

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

