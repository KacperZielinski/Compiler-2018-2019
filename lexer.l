/**
	Kacper Zielinski, 
	236698, 
	Wydzial Podstawowych Problemow Techniki, 
	Politechnika Wroclawska
	
	Kompilator 2018/2019
	
	Lexer
**/

%option noyywrap
%option yylineno

%{
	#include "parser.tab.h"
	#include <string>
	
	extern void yyerror(const char*);
	
%}
%x COMMENT

%%
"["				BEGIN(COMMENT);
<COMMENT>"]"	BEGIN(INITIAL);
<COMMENT>.		;
<COMMENT>\n		;

[ \t]+ 			;
\n				;

"+"				return ADD; 
"-"				return SUB; 
"*"				return MUL; 
"/"				return DIV; 
"%"	 			return MOD; 
"=" 			return EQ;
"!="			return NEQ;
"<"				return LE;
">"				return GE;
"<="			return LEQ;
">="			return GEQ;
":="    		return ASSIGN;
"("				return LB;
")"				return RB;
":"				return COLON;
";"				return SEMICOLON;
"DECLARE"		return DECLARE;
"IN"			return IN;
"END"			return END;
"WRITE"			return WRITE;
"READ"			return READ;
"IF"			return IF;
"THEN"			return THEN;
"ELSE"			return ELSE;
"ENDIF"			return ENDIF;
"WHILE"			return WHILE;
"DO"			return DO;
"ENDDO"			return ENDDO;
"ENDWHILE"		return ENDWHILE;
"FOR"			return FOR;
"FROM"			return FROM;
"TO"			return TO;
"DOWNTO"		return DOWNTO;
"ENDFOR"		return ENDFOR;

[_a-z]+[A-Z0-9]+	{
						std::string lexerError("Invalid identifier: '");
						std::string yyLexerError(yytext);
						lexerError += yyLexerError;
						lexerError += "'";
						yyerror(lexerError.c_str());
					}

[_a-z]+				{ 	
						yylval.str = strdup(yytext); 
						yylval.var.str = strdup(yytext); 
						return pidentifier;
					}
[0-9]+				{ 	
						yylval.str = strdup(yytext); 
						yylval.var.str = strdup(yytext); 
						return num;	
					}

.					{
						std::string lexerError("Unexcepted token: '");
						std::string yyLexerError(yytext);
						lexerError += yyLexerError;
						lexerError += "'";
						yyerror(lexerError.c_str());
					}
%%