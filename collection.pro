domains
intArray = integer *

predicates
split(intArray, intArray, intArray)
numberComparison(integer,integer)
insert_sort(intArray,intArray)
insert(integer,intArray,intArray)

clauses
split([Head|Tail],[Head|L1],L2) :- Head < 0, split(Tail,L1,L2), !. 
split([Head|Tail],L1,[Head|L2]) :- split(Tail,L1,L2), Head >= 0, !.
split([],[],[]).

insert_sort([],[]).
insert_sort([X|Tail],Sorted_list) :- insert_sort(Tail,Sorted_Tail), insert(X,Sorted_Tail,Sorted_list).  /*,nl, write ("X = ", X, ", Sorted_Tail = ", Sorted_Tail, ", Sorted_list = ", Sorted_list), nl, nl.*/
 
insert(X,[Y|Sorted_list],[Y|Sorted_list1]) :- numberComparison(X,Y), !, insert(X,Sorted_list,Sorted_list1).
insert(X,Sorted_list,[X|Sorted_list]). /*:- write (Sorted_list, " + ", X).*/
 
numberComparison(X,Y) :- X>=Y.

goal
Array = [-5,4,0,-15,4,-10,50],
write("��� �����: ", Array),
nl,
nl,
write("��������� ���� �� ������������� � ������������� �����..."),
nl,
split(Array,ArrayMinus,ArrayPlus),
write("������ ������������� �����: ", ArrayMinus),
nl,
write("������ ������������� �����: ", ArrayPlus),
nl,
nl,
write("���������� ����� � ������� �� �����������..."),
nl,
insert_sort(ArrayMinus,ArrayMinusSort),
write("������ ������������� �����: ", ArrayMinusSort),
nl,
insert_sort(ArrayPlus,ArrayPlusSort),
write("������ ������������� �����: ", ArrayPlusSort),
nl,
nl.