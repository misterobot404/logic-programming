domains 
treetype = tree(symbol, treetype, treetype);void  
predicates 
nondeterm isotree (treetype, treetype) 
clauses 
isotree (T, T). 
isotree (tree (X,L1, R1), 
tree (X, L2, R2)):- isotree 
(L1, L2), isotree (R1, 
R2). 
isotree (tree (X, L1, R1), 
tree (X, L2, R2)):- isotree 
(L1, R2), isotree (L2, 
R1). 
 
    goal      
       isotree(tree("a",tree("b",tree("d",void,void),tree("e",void,  
    void)),tree("c",void,void)),      
                tree("a",tree("b",tree("d",void,void),tree("e",void,  
    void)),tree("c",void,void))),  
    write("true").