% ���������: ��������
domains
  slovo, syn = symbol
 
predicates
  % nondeterm ��������, ��� �������� ������� , ���������� �����, ������, ������ � ������ �����, ����� ���� ���������� � ����� ������ (������� � ���������) ����� ���
  nondeterm synonym(slovo,syn)
  nondeterm dictionary�ontent
  nondeterm getSynonym(symbol, symbol)
  
 % �����������:  ������ ������� - ��������� �����, ������ - ��� �������.
clauses
  dictionary�ontent:-
  write("�������:"),
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
  
 % ���������� ����, ��� ����� ���������
goal
  dictionary�ontent,
  write("����� ������� �� �����: "),
  readln(WORD),
  getSynonym(WORD, Syn),
  write(Syn),
  nl,nl.
  
  % ����� ��������� ��������� ���� �� ����� ��������� ������� �������������� ����� � ��������, �������� X = daring
  % ����� ��������� ����������, ��������� ���������� ������� ����