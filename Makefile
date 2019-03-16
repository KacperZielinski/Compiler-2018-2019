program: parser.y lexer.l
	bison -d parser.y
	flex lexer.l
	g++ -std=c++11 -Wno-write-strings -o kompilator lex.yy.c parser.tab.c  
	
clean: 
	rm -f kompilator
	rm -f lex.yy.c
	rm -f parser.tab.c
	rm -f parser.tab.h
