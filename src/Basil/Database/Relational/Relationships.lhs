%if False

> {-# LANGUAGE RankNTypes #-}
> module Basil.Database.Relational.Relationships where

%endif

TODO: this is a problem. TTTAS with subtyping? Talk to Doaitse about this.

To convert relationships, we first start by converting all the entity types into their corresponding database tables. That gives us a datastructure with a typed list of database tables. Now, if we add relationship sets, we have to change the tables.

Consider a one-to-one relationship between entities |L| and |R|. 
In order to reflect this in the relational database, we need to transform the existing tables into a new set of tables.
More specifically, we need to add a foreign key of type |R| to the table generated of |L| entities.
As a result, we get a table structure where the types have changed. Specifically, the type of the table containing |L| entities has changed: an extra field was added.

Recall the type of the |create| method:

> create  :: Table env row -> HList row -> DB Int

After our transformation, |env| has changed into |env'|. We need a way to change the type of |create| methods as well.
Unfortunately, we have not yet implemented this, because we do not know the solution.
We think that the |TTTAS| library \cite{tttas} provides a way to deal with this.

A promising way is to group the operations |create|, |read|, |update| and |delete| into a datatype. For example:

> data Operation env result where
>   Create  :: Ix env row -> HList row  -> Operation env Int
>   Read    :: Ix env row -> Int        -> Operation env (Maybe (HList row))
>   Update  :: Ix env row -> Int        -> HList row -> Operation env ()
>   Delete  :: Ix env row -> Int        -> Operation env ()

Now we can write our transformation in the following way:

\begin{spec}
addRelationship :: Rel env cardL entityL cardR entityR 
                -> DatabaseSchema env
                -> DatabaseTransformation env
\end{spec}

Where |DatabaseTransformation| hides the new |env'| type using existential quantification, and stores the new |DatabaseSchema| as well as a way to convert an |Operation env a| into an |Operation env' a|:

> data DatabaseTransformation env = 
>   DBTrafo (forall env' . (DatabaseSchema env', Operation env result -> Operation env' result))

This should allow for completely changed schemas.
For example, when we add a many-to-many relationship in our database schema, a new table is added.
Therefore, all references to existing tables have to be changed as well.



% We can convert relationships in the same way as we converted entities. A
% relationship set is converted to a table with two columns, one column for each
% entity set involved in the relationship set. Consider the many-to-many
% relationship set \relationship{contributes} defined in section
% \ref{sec:ermodels}: it is converted into a \relationship{contributes} table with
% two attributes, \attrib{compiler\_id} and \attrib{person\_id}, which are both
% foreign keys to the \attrib{id} fields of the \dbtable{compilers} and \dbtable{person}
% tables, respectively.
% 
% For the one-to-one and one-to-many relationships it is possible to take some
% normalization steps. If we consider the \relationship{releases} relationship,
% there is always exactly one \entset{compiler} that belongs to a
% \entset{release}. We can eliminate the table by adding a field
% \attrib{compiler\_id} to the table for the \entset{releases} entity set.
% 
% TODO: write about implementation.
