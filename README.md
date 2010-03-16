# What is Basil?

Basil is a high-level library for data storage in Haskell. It is strongly based on the ideas of entity-relationship modeling: you define your entities. Then you define relationships between those entities. Based on the entities and relationships, you can derive:

* An in-memory database
* An SQL backend
* A NoSQL backend
* ...

The most important reason why the code is sometimes so hard to read is that I want to keep everything typed. Therefore, sometimes we need to store type information explicitly.

## What problems does it solve?

I have the feeling that we can provide a high-level, typed interface to a number of different (possibly untyped) data storage backends. Basil is my experiment to see if this can actually be done.

## What problems does it not solve?

Basil will focus only on the data layer.

## Scratchpad functionality

Basil was inspired by CoreData. The envisioned scratchpad functionality is based on CoreData's Managed Object Context. It would work roughly like this:

* You try to read out the User object with id 1
* This is looked up in the in-memory database. If it's not present, it is looked up in the sqlite database.
* Some objects are changed, relationships are changed, etc. This is only stored in the in-memory database.
* When the state is committed, the changes that are in the in-memory database are saved to the sqlite database.
* Alternatively, 

Of course, if we have a common interface for the in-memory database and the sqlite database we could swap both of them for other components. In fact, we could stack multiple data storage layers on top of each other.

## Current state

The code is very much in alpha state.

## Roadmap

The roadmap is roughly like this:

* Add Sqlite backend
* Build TH for generating all the boilerplate (see Example2.lhs)
* Test heavily
* Write scratchpad functionality

## Possible features

* Transactions 
* Undo/redo support

## Examples

Have a look at Example2.lhs, it is the most recent example.
