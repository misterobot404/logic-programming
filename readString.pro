predicates

nondeterm vv(string)
nondeterm start

clauses

vv(X):-X="",write("������ �� �����").
vv(X):-write("������: ",X),nl,readln(Y),vv(Y).

start:-readln(X),vv(X).

goal

start.

