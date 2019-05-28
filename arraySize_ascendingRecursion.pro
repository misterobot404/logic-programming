domains
sl = symbol*
nl = integer*
predicates
list_len_inp (sl, integer )
list_len_inp (nl, integer )
list_len ( sl, integer, integer )
list_len ( nl, integer, integer )
clauses
list_len_inp ( List, Lengh ):- list_len(List,0,Lengh).
list_len ( [ _ | R ], TempLen, Len) :-  TempLenNew = TempLen + 1, list_len ( R, TempLenNew, Len).
list_len ( [ ], Len, Len).
goal
A=[1,2,3,4,5,6,7,8,9,10],
list_len_inp(A,Len),
write(Len),
nl,
nl.

