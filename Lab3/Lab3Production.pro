domains
intArray = integer*
symArray = symbol*

predicates

insert(integer, integer, intArray, intArray)
insert(symbol, symbol, symArray, symArray)

revers(intArray, intArray)
revers(intArray, intArray, intArray)
revers(symArray, symArray)
revers(symArray, symArray, symArray)

task3_entry()
append(integer, integer, intArray, intArray, intArray, intArray)
append(intArray,intArray,intArray)
append(integer, integer, symArray, symArray, symArray, symArray)
append(symArray,symArray,symArray)

insert_sort(intArray,intArray)
insertEl(integer,intArray,intArray)

clauses
%Task 1
insert(_,_,[],[]).
insert(X,Head,[Head|Z],[Head,X|T]):- insert(X,Head,Z,T),!.
insert(X,Y,[Head|Z],[Head|N]):- insert(X,Y,Z,N).
%Task 2 
revers(X,Y):- revers([],X,Y).  
revers(Y,[],Y).  
revers(X1,[Z|X2],Y):- revers([Z|X1],X2,Y).
%Task 3
task3_entry():- 
%num
write("Списки:"), nl,
NumArray1 = [-5,4,0,-15,4],
NumArray2 = [0,1,2],
NumArray3 = [-1,-2,-3],
write(NumArray1), nl,
write(NumArray2), nl,
write(NumArray3), nl,
write("Введите порядок присоединения списков (через Enter):"), nl,
readint(FirstNum),
readint(SecondNum),
append(FirstNum, SecondNum, NumArray1, NumArray2, NumArray3, NumArray3Res),
write("Результат: ", NumArray3Res), nl, 
%sym
write("Списки:"), nl,
SymArray1 = ["g","y","g","b"],
SymArray2 = ["y"],
SymArray3 = ["g","b"],
write(SymArray1), nl,
write(SymArray2), nl,
write(SymArray3), nl,
write("Введите порядок присоединения списков (через Enter):"), nl,
readint(FirstSym),
readint(Second1Sym),
append(FirstSym, Second1Sym, SymArray1, SymArray2, SymArray3, SymArray3Res),
write("Результат: ", SymArray3Res), nl, nl.

append([], L, L).
append([N|L1], L2, [N|L3]) :- append(L1,L2,L3).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=1, Second=2, !, append(L1, L2, Res), append(Res, L3, L4).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=1, Second=3, !, append(L1, L3, Res), append(Res, L2, L4).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=2, Second=1, !, append(L2, L1, Res), append(Res, L3, L4).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=2, Second=3, !, append(L2, L3, Res), append(Res, L1, L4).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=3, Second=1, !, append(L3, L1, Res), append(Res, L2, L4).
append(Fisrt, Second, L1, L2, L3, L4) :- Fisrt=3, Second=2, !, append(L3, L2, Res), append(Res, L1, L4).
%Task 4
insert_sort([],[]).
insert_sort([X|Tail],Sorted_list) :- insert_sort(Tail,Sorted_Tail), insertEl(X,Sorted_Tail,Sorted_list).
 
insertEl(X,[Y|Sorted_list],[Y|Sorted_list1]) :-  X<Y, !, insertEl(X,Sorted_list,Sorted_list1).
insertEl(X,Sorted_list,[X|Sorted_list]).

goal
nl, write("1 ВСТАВКА ЭЛЕМЕНТА В СПИСОК ПОСЛЕ ВСЕХ ВХОЖДЕНИЙ УКАЗАННОГО"), nl,
%num
NumArray = [-5,4,0,-15,4,-10,50],
write("Список: ", NumArray), nl,
write("Введите элемент, который необходимо вставить: "),
readint(Insert_number),
write("Введите элемент, после которого необходимо вставить: "),
readint(After_number),
insert(Insert_number, After_number, NumArray, NumArrayRes),
write("Результат: ", NumArrayRes), nl,
%sym
SymArray = ["g","y","g","b"],
write("Список: ", SymArray), nl,
write("Введите элемент, который необходимо вставить: "),
readln(Insert_symbol),
write("Введите элемент, после которого необходимо вставить: "),
readln(After_symbol),
insert(Insert_symbol, After_symbol, SymArray, SymArrayRes),
write("Результат: ", SymArrayRes), nl,nl,

write("2 РЕВЕРС СПИСКА"), nl,
%num
write("Список: ", NumArray), nl,
revers(NumArray, NumArray2Res),
write("Результат: ", NumArray2Res), nl,
%sym
write("Список: ", SymArray), nl,
revers(SymArray, SymArray2Res),
write("Результат: ", SymArray2Res), nl,nl,

write("3 ОБЪЕДИНЕНИЕ ТРЁХ СПИСКОВ В ОДИН В УКАЗАННОМ ПОРЯДКЕ"), nl,
task3_entry(),

write("4 СОРТИРОВКА ПО УБЫВАНИЮ МЕТОДОМ ВСТАВОК"),nl,
write("Исходный список: ", NumArray),nl,
insert_sort(NumArray,Array4Res),
write("Результат: ", Array4Res),nl, nl.