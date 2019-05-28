% Программа: Синонимы
domains
  slovo, syn = symbol
 
predicates
  % nondeterm означает, что предикат правило , содержащий целое, строку, строку и список целых, может быть согласован с базой знаний (фактами и правилами) много раз
  nondeterm synonym(slovo,syn)
  nondeterm dictionaryСontent
  nondeterm getSynonym(symbol, symbol)
  
 % Утверждения:  первая позиция - некоторое слово, вторая - его синоним.
clauses
  dictionaryСontent:-
  write("Словарь:"),
  nl,  
  write("brave"),
  nl,
  write("daring"),
  nl,
  write("honest"),
  nl,
  write("truthful"),
  nl,
  write("modern"),
  nl,
  write("new"),
  nl,
  write("rare"),
  nl,
  write("uncommon"),
  nl.

  getSynonym(Word, Syn):- synonym(Word,Syn).
  getSynonym(Word, Syn):- synonym(Syn,Word).

  synonym(brave,daring).
  synonym(honest,truthful).
  synonym(modern,new).
  synonym(rare,uncommon).
  
 % Внутренняя цель, что нужно проверить
goal
  dictionaryСontent,
  write("Найти синоним по слову: "),
  readln(WORD),
  getSynonym(WORD, Syn),
  write(Syn),
  nl,nl.
  
  % Когда программа достигнет цели на экран выведутся неявные преобразования типов к обьектам, например X = daring
  % Когда программа завершится, выведется количество решений цели