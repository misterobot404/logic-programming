DOMAINS
treetype=tree (integer, treetype, treetype); void

PREDICATES
nondeterm search_tree (integer, treetype)

CLAUSES
search_tree (Number, tree(_, Left, Right)):- search_tree (Number, Left);search_tree (Number, Right),!.
search_tree(Number,tree(Root,void,void)):- Number=Root, write("Найдено число: ", Number), nl,!.                                    

GOAL
write("Введите число для поиска в дереве: "),
readint(Number),
search_tree (Number, tree (4, tree (2, tree (1, void, void), 
            tree (3, void, void)), tree (5, void, void))).