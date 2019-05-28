predicates

nondeterm vv(string)
nondeterm start

clauses

vv(X):-X="",write("Ничего не ввели").
vv(X):-write("Строка: ",X),nl,readln(Y),vv(Y).

start:-readln(X),vv(X).

goal

start.

