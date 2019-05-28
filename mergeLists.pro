/* Программа «Присоединение (слияние) списков». */

domains
intArray = integer *

predicates
append(intArray,intArray,intArray)

clauses
append([], L, L).
append([N|L1], L2, [N|L3]) :- append(L1,L2,L3).

goal
append([1,2,3],[4,5],L).