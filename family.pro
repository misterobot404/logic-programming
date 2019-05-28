domains
  person = symbol
 
predicates
  nondeterm male(person)
  nondeterm female(person)
  nondeterm parents(person,person,person)
  nondeterm sister(person,person)
 
  nondeterm who_is_the_sister
  nondeterm who_is_the_son
 
clauses
  % Факты
  male("Frank"):-write("male(Frank)"),nl.
  male("Sam"):-write("male(Sam)"),nl.
  female("Mary"):-write(" female(Mary)"),nl.
  female("Debbie"):-write("female(Debbie)"),nl.
  parents("Sam","Frank","Mary"):-write("parents(Sam,Frank,Mary)"),nl.
  parents("Debbie","Frank","Mary"):-write("parents(Debbie,Frank,Mary)"),nl.
 
  % Правило задающее отношение брат-сестра
  sister(Sister,Brother) :- 
  
  female(Sister),
  write("Sister - ", Sister),
  nl,
   
  male(Brother),
  write("Brother - ", Brother),
  nl,
   
  parents(Sister,Father,Mother),
  write("Sister - ", Sister, " Father - ", Father, " Mother - ", Mother),
  nl,
   
  parents(Brother,Father,Mother),
  write("Brother - ", Brother, " Father - ", Father, " Mother - ", Mother),
  nl.
 
  % Правило нулевой арнасти для нахождения сестры
  who_is_the_sister :- 
  sister(Sister,Brother), 
  write(Sister," is the sister of ",Brother, "."),
  nl.
 
  % Правило нулевой арнасти для нахождения сына
  who_is_the_son :- 
  parents(Son,Father,Mother), 
  male(Son), 
  write("The son is ",Son,"."),
  nl.
  
goal
  % Цель это предикат
  who_is_the_sister,
  who_is_the_son.