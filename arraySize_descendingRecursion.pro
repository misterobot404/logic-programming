domains
sl = symbol*
nl = integer*
predicates
list_len ( sl, integer )
list_len ( nl, integer )
clauses
list_len ( [ ], _ ).
list_len ( [ _ | R ], LEN ) :- list_len ( R, LEN1 ), LEN = LEN1 + 1.
goal
A=[1,2,3,4,5,6,7,8,9,10],
list_len(A,Len),
write(Len),
nl,
nl.
