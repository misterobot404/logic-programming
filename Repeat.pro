predicates
start
ic(integer)
nondeterm rep

clauses
ic(X):-X=0, write("End"), !.
ic(X):-write(X),fail.
start:-rep,readint(X),ic(X),!.
rep.
rep:-rep.

goal
start.
