yacc -d AST.y
lex AST.l
gcc -g y.tab.c lex.yy.c graph.c -ll
