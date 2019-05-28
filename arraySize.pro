domains
intArray = integer *

predicates
arraySize(intArray, integer)

clauses
arraySize([], Count):- write(Count).
arraySize([_|Tail], Count):- CountPlusOne = Count + 1, !, arraySize(Tail, CountPlusOne).

goal
arraySize([4,2,-1,5,5], 0).