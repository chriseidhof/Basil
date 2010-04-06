%if False

> module Basil.Database.Relational.Relationships where

%endif

TODO: this is a problem. TTTAS with subtyping? Talk to Doaitse about this.

To convert relationships, we first start by converting all the entities into their corresponding database tables.
The entities are grouped into a typed environment |Env|.

On an abstract level, we proceed as following:

\begin{itemize}
\item To implement a one-to-one relationship between entities of |l| and |r|, we add a foreign key |r| to the database table of |l|.
\item To implement a one-to-many relationship between entities of |l| and |r|, we add a foreign key |l| to the database table of |r|.
\item To implement a many-to-many relationship, we introduce a new table with foreign keys to the tables for both entities.
\end{itemize}


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
