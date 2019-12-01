/**
	Kacper Zielinski, 
	236***, 
	Wydzial Podstawowych Problemow Techniki, 
	Politechnika Wroclawska
	
	Kompilator 2018/2019
	
	Parser
**/

%{	
	#include <iostream>
	#include <fstream>
	#include <string>
	#include <cstring>
	#include <vector>
	#include <stack>
	#include <deque>
	#include <unordered_map>
	
	extern int yylineno;
	extern FILE* yyin;
	void yyerror(const char* errorMessage);
	int yylex();
	
	const char ARRAY = 'A';
	const char ITERATOR = 'I';
	const char VARIABLE = 'V';
	const short REGISTERS = 8;
	std::string ITERATOR_COUNTER_PREFIX = "69";
	
	unsigned long long memoryOffset = 0;

	void generateOutputNum(unsigned long long num, std::string reg);
	void generateNegation(std::string reg);
	void generateNegation(std::string reg, std::string reg2);
	void storePreviousLoopValues(char* name);
	void loadLoopValuesToRegistersByName(char* name);

	char* convertStringToCharPointer(std::string str);

	void addVariableToSymbolsMap(char* variableName, char type);
	void addVariableToSymbolsMap(char* variableName, char type, unsigned long long start, unsigned long long end);
	void removeVariableFromSymbolsMap(char* variableName);
	
	void errorMessage(std::string messageBegin, char* variableName, std::string messageEnd);
	void errorMessage(std::string messageBegin, std::string variableName, std::string messageEnd);
	void pushOutputCommand(std::string opCode);
	void pushOutputCommand(std::string opCode, std::string arg1);
	void pushOutputCommand(std::string opCode, std::string arg1, std::string arg2);
	void pushOutputCommand(std::string opCode, unsigned long long arg1);
	void pushOutputCommand(std::string opCode, std::string arg1, unsigned long long arg2);
	void printOutputCommands();
	
	std::stack<unsigned long long> doWhileJumpStack;
	std::stack<unsigned long long> whileJumpStack;
	std::stack<unsigned long long> ifJumpStack;
	std::stack<unsigned long long> forJumpStack;
	
	std::stack<char*> iteratorNamesStack;
	std::stack<std::string> iteratorRegisterNamesStack;
	
	std::vector<std::string> outputCommands;
	
	unsigned long long outputCommandsSize = 0;
	unsigned long long whileCommandsDecreaser = 0;
%}
%error-verbose

%token <str> pidentifier "variable [_a-z]";
%token <str> num "number [0-9]";
%token <str> ADD "`+`"; 
%token <str> SUB "`-`"; 
%token <str> MUL "`*`"; 
%token <str> DIV "`/`"; 
%token <str> MOD "`%`"; 
%token <str> EQ "`=`";
%token <str> NEQ "`!=`";
%token <str> LE "`<`";
%token <str> GE "`>`";
%token <str> LEQ "`<=`";
%token <str> GEQ "`>=`";
%token <str> ASSIGN "`:=`"; 
%token <str> LB "`(`";
%token <str> RB "`)`";
%token <str> COLON "`:`";
%token <str> SEMICOLON "`;`";
%token <str> DECLARE;
%token <str> IN;
%token <str> END;
%token <str> WRITE;
%token <str> READ;
%token <str> IF;
%token <str> THEN;
%token <str> ELSE;
%token <str> ENDIF;
%token <str> WHILE;
%token <str> DO;
%token <str> ENDDO;
%token <str> ENDWHILE;
%token <str> FOR;
%token <str> FROM;
%token <str> TO;
%token <str> DOWNTO;
%token <str> ENDFOR;
%token <str> ERROR;

%code requires {
	
	typedef struct {
		char* str;
		char type;
		bool isNumber;
		bool isInitialized;
		bool isConstantReference;
		bool isVariableReference;
		char* reference;
		unsigned long long index;
		unsigned long long start;
		unsigned long long end;
		unsigned long long memoryOffsetBegin;
		unsigned long long memoryOffsetEnd;
		
	} VariableStruct;

	VariableStruct* getVariableByName(char* variableName);
	void initializeVariableByName(char* variableName);
	void initializeVariable(VariableStruct* var);
	
	void handleAssignOperation(VariableStruct& $1, VariableStruct& $3);
	void handleArrayReferenceWithIdentifier(VariableStruct& varStruct, char* $1, char* $3);
	void handleArrayReferenceWithNum(VariableStruct& varStruct, char* $1, char* $3);
	void handlePrimitiveIdentifier(VariableStruct& varStruct, char* $1);
	void handleReadOperation(VariableStruct& $2);
	void handleWriteOperation(VariableStruct& $2);
	
	void multiply(VariableStruct& $1, VariableStruct& $3);
	void divide(VariableStruct& $1, VariableStruct& $3);
	void modulo(VariableStruct& $1, VariableStruct& $3);
	
	void loadVariableToRegister(VariableStruct& var, char* reg);
	unsigned long long loadVariableToRegisterWithCommandsSizeDiff(VariableStruct& varStruct, char* reg);
}

%union {	
	char* str;
	VariableStruct var;
}

%type <var> identifier;
%type <var> expression;
%type <var> value;
%type <var> command;
%type <var> condition;

%%

program:
        DECLARE declarations IN commands END {
			pushOutputCommand("HALT");
        }
    ;
	
declarations:
        declarations pidentifier SEMICOLON {
			addVariableToSymbolsMap($2, VARIABLE);
        }
    |   declarations pidentifier LB num COLON num RB SEMICOLON {	
			unsigned long long left = atoll($4);
			unsigned long long right = atoll($6);
			
			if(left > right) {
				errorMessage("Array declaration '", $2, "' invalid, start index cannot be greater than the end one");
			} else {
				addVariableToSymbolsMap($2, ARRAY, left, right);
			}
        }
    |
    ;
	
commands:
	commands command
|	command
;

command:
        identifier ASSIGN expression SEMICOLON {
			handleAssignOperation($<var>1, $<var>3);
		}		
	|	IF condition THEN {
			ifJumpStack.push(outputCommandsSize);
			pushOutputCommand("should_be_replaced_by_jump_if");
			whileCommandsDecreaser = 0;
	}	commands ifbody {
		
		}
	|	WHILE condition DO {
			whileJumpStack.push(outputCommandsSize);
			whileJumpStack.push(whileCommandsDecreaser);
			pushOutputCommand("should_be_replaced_by_jzero_while");
			whileCommandsDecreaser = 0;
		} commands ENDWHILE {
			int decreaser = whileJumpStack.top();
			whileJumpStack.pop();
			
			int index = whileJumpStack.top();
			whileJumpStack.pop();

			outputCommands[index] = "JZERO H " + std::to_string(outputCommandsSize + 1);			
			pushOutputCommand("JUMP", index - decreaser);
		}	
	|   DO  {
			doWhileJumpStack.push(outputCommandsSize);
		} commands WHILE condition ENDDO {
			int index = doWhileJumpStack.top();
			doWhileJumpStack.pop();
			pushOutputCommand("JZERO", "H", outputCommandsSize+2);
			pushOutputCommand("JUMP", std::to_string(index));
			whileCommandsDecreaser = 0;
		}
    |   FOR pidentifier FROM value TO value DO {
			addVariableToSymbolsMap($2, ITERATOR);
			
			std::string counterName(ITERATOR_COUNTER_PREFIX);
			counterName += $2;
			addVariableToSymbolsMap(convertStringToCharPointer(counterName), ITERATOR);	
			
			if(!iteratorNamesStack.empty()) {
				storePreviousLoopValues(iteratorNamesStack.top());
			}
			
			iteratorNamesStack.push($2);
			VariableStruct* iterator = getVariableByName($2);
			
			VariableStruct* var4 = nullptr;
			VariableStruct* var6 = nullptr;
			
			if($4.isNumber && $6.isNumber) {
				unsigned long long number4 = atoll($4.str);
				unsigned long long number6 = atoll($6.str);
				
				if(number4 > number6) {
					pushOutputCommand("SUB", "D", "D");
					pushOutputCommand("SUB", "E", "E");
				} else {
					unsigned long long iterationAmount = number6 + 2 - number4;	
					generateOutputNum(number4, "D");
					generateOutputNum(iterationAmount, "E");
				}
			} else {
				VariableStruct* var4 = getVariableByName($4.str);
				VariableStruct* var6 = getVariableByName($6.str);
					
				if(var4 != nullptr && var4->str == $2) {
					errorMessage("Cannot use the same iterator '", $2, "' more than one time in a loop declaration");
				}

				if(var6 != nullptr && var6->str == $2) {
					errorMessage("Cannot use the same iterator '", $2, "' more than one time in a loop declaration");
				}
				
				loadVariableToRegister($<var>4, "D");
				loadVariableToRegister($<var>6, "E");
			
				pushOutputCommand("INC", "E");
				pushOutputCommand("INC", "E");
				pushOutputCommand("SUB", "E", "D");
			}

			forJumpStack.push(outputCommandsSize);
			pushOutputCommand("DEC", "E");
			pushOutputCommand("should_be_replaced_by_jump_for_to");
			
		} commands ENDFOR {
			int index = forJumpStack.top() + 1;		
			forJumpStack.pop();

			outputCommands[index] = "JZERO E " + std::to_string(outputCommandsSize + 2);
			pushOutputCommand("INC", "D");
			pushOutputCommand("JUMP", index - 1);
			
			char* currentIteratorName = iteratorNamesStack.top();
			std::string currentIteratorConstCounterName(ITERATOR_COUNTER_PREFIX);
			currentIteratorConstCounterName += currentIteratorName;
			
			removeVariableFromSymbolsMap(currentIteratorName);
			removeVariableFromSymbolsMap(convertStringToCharPointer(currentIteratorConstCounterName));
			iteratorNamesStack.pop();
			
			if(!iteratorNamesStack.empty()) {
				loadLoopValuesToRegistersByName(iteratorNamesStack.top());
			}
        }	
    |   FOR pidentifier FROM value DOWNTO value DO {
			addVariableToSymbolsMap($2, ITERATOR);
			
			std::string counterName(ITERATOR_COUNTER_PREFIX);
			counterName += $2;
			addVariableToSymbolsMap(convertStringToCharPointer(counterName), ITERATOR);	
			
			if(!iteratorNamesStack.empty()) {
				storePreviousLoopValues(iteratorNamesStack.top());
			}

			VariableStruct* iterator = getVariableByName($2);

			if($4.isNumber && $6.isNumber) {
				unsigned long long number4 = atoll($4.str);
				unsigned long long number6 = atoll($6.str);
				
				if(number4 < number6) {
					pushOutputCommand("SUB", "D", "D");
					pushOutputCommand("SUB", "E", "E");
				} else {
					unsigned long long iterationAmount = number4 + 2 - number6;	
					generateOutputNum(number4, "D");
					generateOutputNum(iterationAmount, "E");
				}
			} else {
				VariableStruct* var4 = getVariableByName($4.str);
				VariableStruct* var6 = getVariableByName($6.str);
					
				if(var4 != nullptr && var4->str == $2) {
					errorMessage("Cannot use the same iterator '", $2, "' more than one time in a loop declaration");
				}

				if(var6 != nullptr && var6->str == $2) {
					errorMessage("Cannot use the same iterator '", $2, "' more than one time in a loop declaration");
				}
				
				loadVariableToRegister($<var>4, "D");
				loadVariableToRegister($<var>6, "E");

				pushOutputCommand("COPY", "C", "D");
				pushOutputCommand("INC", "D");
				pushOutputCommand("INC", "D");
				pushOutputCommand("SUB", "D", "E");
				pushOutputCommand("COPY", "E", "D");
				pushOutputCommand("COPY", "D", "C");
			}
			iteratorNamesStack.push($2);
			forJumpStack.push(outputCommandsSize);
			pushOutputCommand("DEC", "E");
			pushOutputCommand("should_be_replaced_by_jump_for_downto");
			
		} commands ENDFOR {
			int index = forJumpStack.top() + 1;		
			forJumpStack.pop();

			outputCommands[index] = "JZERO E " + std::to_string(outputCommandsSize + 2);
			pushOutputCommand("DEC", "D");
			pushOutputCommand("JUMP", index - 1);
			
			char* currentIteratorName = iteratorNamesStack.top();
			std::string currentIteratorConstCounterName(ITERATOR_COUNTER_PREFIX);
			currentIteratorConstCounterName += currentIteratorName;
			
			removeVariableFromSymbolsMap(currentIteratorName);
			removeVariableFromSymbolsMap(convertStringToCharPointer(currentIteratorConstCounterName));
			iteratorNamesStack.pop();
			
			if(!iteratorNamesStack.empty()) {
				loadLoopValuesToRegistersByName(iteratorNamesStack.top());
			}
        }
    |   READ identifier SEMICOLON {
			handleReadOperation($<var>2);

		}
	|   WRITE value SEMICOLON {
			handleWriteOperation($<var>2);
		}
    ;

ifbody:
		ELSE {
			int index = ifJumpStack.top();
			ifJumpStack.pop();
			ifJumpStack.push(outputCommandsSize);
			pushOutputCommand("should_be_replaced_by_jump_if_else");
			outputCommands[index] = "JZERO H " + std::to_string(outputCommandsSize);
			whileCommandsDecreaser = 0;
			
		} commands ENDIF {
			int index = ifJumpStack.top();
			ifJumpStack.pop();
			outputCommands[index] = "JUMP " + std::to_string(outputCommandsSize);
			whileCommandsDecreaser = 0;
		}
	|   ENDIF {
			int index = ifJumpStack.top();
			ifJumpStack.pop();
			outputCommands[index] = "JZERO H " + std::to_string(outputCommandsSize);			
			whileCommandsDecreaser = 0;
		}
;

expression:
        value {
			if($1.isNumber) {
				generateOutputNum(atoll($1.str), "H");
			} else {
				loadVariableToRegister($<var>1, "H");
			}
		}
    |   value ADD value {
			if($1.isNumber && $3.isNumber) {
				unsigned long long score = atoll($1.str) + atoll($3.str);
				generateOutputNum(score, "H");
			} else {
				if($<var>1.str == $<var>3.str
					&& $<var>1.reference == $<var>3.reference
					&& $<var>1.isConstantReference == $<var>3.isConstantReference
					&& $<var>1.isVariableReference == $<var>3.isVariableReference) {
						
					loadVariableToRegister($<var>1, "H");
					pushOutputCommand("ADD", "H", "H");
					
				} else {
					loadVariableToRegister($<var>1, "H");
					loadVariableToRegister($<var>3, "B");
					pushOutputCommand("ADD", "H", "B");
				}
			}
		}
    |   value SUB value {
			if($1.isNumber && $3.isNumber) {
				unsigned long long number1 = atoll($1.str);
				unsigned long long number3 = atoll($3.str);
				
				if(number1 < number3) {
					pushOutputCommand("SUB H H");
				} else {
					generateOutputNum(number1 - number3, "H");
				}
				
			} else {
				if($<var>1.str == $<var>3.str
					&& $<var>1.reference == $<var>3.reference
					&& $<var>1.isConstantReference == $<var>3.isConstantReference
					&& $<var>1.isVariableReference == $<var>3.isVariableReference) {
						
					pushOutputCommand("SUB", "H", "H");
					
				} else {
					loadVariableToRegister($<var>1, "H");
					loadVariableToRegister($<var>3, "B");
					pushOutputCommand("SUB", "H", "B");
				}
			}
		}
    |   value MUL value {
			if($1.isNumber && $3.isNumber) {
				unsigned long long score = atoll($1.str) * atoll($3.str);
				generateOutputNum(score, "H");
			} else  {
				multiply($<var>1, $<var>3);
			}
		}
    |   value DIV value {
			if($1.isNumber && $3.isNumber) {
				unsigned long long number1 = atoll($1.str);
				unsigned long long number3 = atoll($3.str);
				
				if(number3 == 0) {
					pushOutputCommand("SUB", "H", "H");
				} else {
					unsigned long long score = atoll($1.str) / atoll($3.str);
					generateOutputNum(score, "H");
				}
			} else {
				if($1.isNumber && atoll($1.str) == 0 || $3.isNumber && atoll($3.str) == 0) {
					pushOutputCommand("SUB", "H", "H");
				} else if($3.isNumber && atoll($3.str) == 1) {
					loadVariableToRegister($<var>1, "H");
				} else if($<var>1.str == $<var>3.str
					&& $<var>1.reference == $<var>3.reference
					&& $<var>1.isConstantReference == $<var>3.isConstantReference
					&& $<var>1.isVariableReference == $<var>3.isVariableReference) {

					loadVariableToRegister($<var>1, "B");
					pushOutputCommand("SUB", "H", "H");
					pushOutputCommand("JZERO", "B", outputCommandsSize+2);
					pushOutputCommand("INC", "H");
				} else {
					divide($<var>1, $<var>3);
				}
			}
		}
    |   value MOD value {
			if($1.isNumber && $3.isNumber) {
				unsigned long long number1 = atoll($1.str);
				unsigned long long number3 = atoll($3.str);
				
				if(number3 == 0) {
					pushOutputCommand("SUB", "H", "H");
				} else {
					unsigned long long score = atoll($1.str) % atoll($3.str);
					generateOutputNum(score, "H");
				}
			} else {
				if($1.isNumber && atoll($1.str) == 0 
					|| ($3.isNumber && (atoll($3.str) == 0 || atoll($3.str) == 1)) 
					|| ($<var>1.str == $<var>3.str && $<var>1.reference == $<var>3.reference 
						&& $<var>1.isConstantReference == $<var>3.isConstantReference
						&& $<var>1.isVariableReference == $<var>3.isVariableReference)) {		
					pushOutputCommand("SUB", "H", "H");
				} else {
					modulo($<var>1, $<var>3);
				}	
			}
		}
    ;

condition:
        value EQ value  {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) == atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				} 
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "B");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "H");
				pushOutputCommand("COPY", "C", "B");
				pushOutputCommand("SUB", "B", "H");
				pushOutputCommand("SUB", "H", "C");
				pushOutputCommand("ADD", "H", "B");
				generateNegation("H");
				whileCommandsDecreaser += 8;
			}
		}
    |   value NEQ value {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) != atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				} 
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "B");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "H");
				pushOutputCommand("COPY", "C", "B");
				pushOutputCommand("SUB", "B", "H");
				pushOutputCommand("SUB", "H", "C");
				pushOutputCommand("ADD", "H", "B");
				whileCommandsDecreaser += 4;
			}
			
		}
    |   value LE value  {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) < atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				} 
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "B");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "H");
				pushOutputCommand("SUB", "H", "B");
				whileCommandsDecreaser++;
			}
		}
    |   value GE value  {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) > atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				}
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "H");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "B");
				pushOutputCommand("SUB", "H", "B");
				whileCommandsDecreaser++;
			}
		}
    |   value LEQ value {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) <= atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				}
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "H");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "B");
				pushOutputCommand("SUB", "H", "B");
				whileCommandsDecreaser++;
				generateNegation("H");
				whileCommandsDecreaser += 4;
			}
		}
    |   value GEQ value {
			if($1.isNumber && $3.isNumber) {
				pushOutputCommand("SUB", "H", "H");
				whileCommandsDecreaser++;
				
				if(atoll($1.str) >= atoll($3.str)) {
					pushOutputCommand("INC", "H");
					whileCommandsDecreaser++;
				}
			} else {
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>1, "B");
				whileCommandsDecreaser += loadVariableToRegisterWithCommandsSizeDiff($<var>3, "H");
				pushOutputCommand("SUB", "H", "B");
				whileCommandsDecreaser++;
				generateNegation("H");
				whileCommandsDecreaser += 4;
			}
		}
    ;

value:
        num {
			$<var>$.isNumber = true;
		}
    |   identifier {
			$<var>$.isNumber = false; 
		}
    ;

identifier:
        pidentifier {
			handlePrimitiveIdentifier($<var>$, $1);
        }
    |   pidentifier LB pidentifier RB {
			handleArrayReferenceWithIdentifier($<var>$, $1, $3);
		}
    |   pidentifier LB num RB {
			handleArrayReferenceWithNum($<var>$, $1, $3);
		}
    ;
%%


std::unordered_map<std::string, VariableStruct> symbolsMap;
std::unordered_map<std::string, VariableStruct>::iterator symbolsMapIterator;


void addVariableToSymbolsMap(char* variableName, char type) {
	addVariableToSymbolsMap(variableName, type, 0, 0);
}

void addVariableToSymbolsMap(char* variableName, char type, unsigned long long start, unsigned long long end) {
	std::string name(variableName);
	
	if(symbolsMap.find(name) != symbolsMap.end()) {
		errorMessage("Another variable declaration '", variableName, "' is not allowed");
	} else {
		VariableStruct var; 
		var.str = variableName;
		var.isInitialized = false;
		var.isConstantReference = false;
		var.isVariableReference = false;
		var.reference = "";
		var.isNumber = false;
		var.type = type;
		var.index = 0;
		var.start = start;
		var.end = end;
		var.memoryOffsetBegin = memoryOffset;
		var.memoryOffsetEnd = memoryOffset;
		
		switch(var.type) {
			case ARRAY: {
				initializeVariable(&var);
				memoryOffset = memoryOffset + (end - start);
				var.memoryOffsetEnd = memoryOffset;
				break;
			}
			
			case ITERATOR: {
				var.isInitialized = true;
				break;
			}
			
			case VARIABLE: {
				break;
			}
			
			default: {
				yyerror("Bad variable 'type' while adding to symbol map");
			}
		}
		memoryOffset++;		
		symbolsMap[name] = var;
	}
}

void removeVariableFromSymbolsMap(char* variableName) {
	std::string name(variableName);
	VariableStruct* variable = getVariableByName(variableName);
	
	symbolsMapIterator = symbolsMap.find(name);
	symbolsMap.erase(symbolsMapIterator);
}

void handleAssignOperation(VariableStruct& $1, VariableStruct& $3) {
	VariableStruct* var1 = getVariableByName($1.str);
	
	if(var1->type == ARRAY) {
		if($1.isConstantReference) {
			unsigned long long variableMemoryOffset = var1->memoryOffsetBegin + atoll($1.reference) - (var1->start);
			generateOutputNum(variableMemoryOffset, "A");
			pushOutputCommand("STORE", "H");
		} else if($1.isVariableReference) {	
			VariableStruct* referencedVariable = getVariableByName($1.reference);

			if(referencedVariable->type == ITERATOR && !iteratorNamesStack.empty() && referencedVariable->str == iteratorNamesStack.top()) {
				pushOutputCommand("COPY", "B", "D");
			} else {
				generateOutputNum(referencedVariable->memoryOffsetBegin, "A");
				pushOutputCommand("LOAD", "B");
			}
			
			if(var1->memoryOffsetBegin > var1->start) {		
				generateOutputNum(var1->memoryOffsetBegin - var1->start, "A");
				pushOutputCommand("ADD", "A", "B");
			} else if (var1->memoryOffsetBegin < var1->start) {
				generateOutputNum(var1->start, "A");
				pushOutputCommand("SUB", "B", "A");
				generateOutputNum(var1->memoryOffsetBegin, "A");
				pushOutputCommand("SUB", "B", "A");
				pushOutputCommand("COPY", "A", "B");
			} else {
				pushOutputCommand("COPY", "A", "B");
			}
			pushOutputCommand("STORE", "H");
		}
	} else if(var1->type == ITERATOR) {
		errorMessage("It's not allowed to assign iterator '", $1.str, "' again");
	} else {	
		initializeVariable(var1);
		generateOutputNum(var1->memoryOffsetBegin, "A");
		pushOutputCommand("STORE", "H");
	}
}

void multiply(VariableStruct& $1, VariableStruct& $3) {
	pushOutputCommand("SUB", "H", "H");
	loadVariableToRegister($1, "B");
	loadVariableToRegister($3, "F");
	
	unsigned long long line = outputCommandsSize;
	pushOutputCommand("JODD", "F", line+5);
	pushOutputCommand("ADD", "B", "B");
	pushOutputCommand("HALF", "F");
	pushOutputCommand("JZERO", "F", line+7);
	pushOutputCommand("JUMP", line);
	pushOutputCommand("ADD", "H", "B");
	pushOutputCommand("JUMP", line+1);
}

void divide(VariableStruct& $1, VariableStruct& $3) {
	loadVariableToRegister($3, "F");
	
	unsigned long long line = outputCommandsSize;
	pushOutputCommand("should_be_replaced_by_divide");
	unsigned long long diff = loadVariableToRegisterWithCommandsSizeDiff($1, "B");
	outputCommands[line] = "JZERO F " + std::to_string(line + diff + 23);
	line += diff;

	pushOutputCommand("SUB H H");
	pushOutputCommand("SUB G G");
	pushOutputCommand("INC G");
	pushOutputCommand("COPY A B");
	pushOutputCommand("SUB A F");
	pushOutputCommand("JZERO A", line+10);
	pushOutputCommand("ADD F F");
	pushOutputCommand("ADD G G");
	pushOutputCommand("JUMP", line+4);
	pushOutputCommand("COPY A F");
	pushOutputCommand("SUB A B");
	generateNegation("A", "A");
	pushOutputCommand("JZERO A", line+19);
	pushOutputCommand("SUB B F");
	pushOutputCommand("ADD H G");
	pushOutputCommand("HALF F");
	pushOutputCommand("HALF G");
	pushOutputCommand("JZERO G", line+24);
	pushOutputCommand("JUMP", line+10);
	pushOutputCommand("SUB H H");	
}

void modulo(VariableStruct& $1, VariableStruct& $3) {
	loadVariableToRegister($3, "F");
	
	unsigned long long line = outputCommandsSize;
	pushOutputCommand("should_be_replaced_by_divide");
	unsigned long long diff = loadVariableToRegisterWithCommandsSizeDiff($1, "B");
	outputCommands[line] = "JZERO F " + std::to_string(line + diff + 23);
	line += diff;

	pushOutputCommand("SUB H H");
	pushOutputCommand("SUB G G");
	pushOutputCommand("INC G");
	pushOutputCommand("COPY A B");
	pushOutputCommand("SUB A F");
	pushOutputCommand("JZERO A", line+10);
	pushOutputCommand("ADD F F");
	pushOutputCommand("ADD G G");
	pushOutputCommand("JUMP", line+4);
	pushOutputCommand("COPY A F");
	pushOutputCommand("SUB A B");
	generateNegation("A", "A");
	pushOutputCommand("JZERO A", line+19);
	pushOutputCommand("SUB B F");
	pushOutputCommand("ADD H G");
	pushOutputCommand("HALF F");
	pushOutputCommand("HALF G");
	pushOutputCommand("JZERO G", line+24);
	pushOutputCommand("JUMP", line+10);
	pushOutputCommand("SUB B B");
	pushOutputCommand("COPY H B");
}
	
void handleWriteOperation(VariableStruct& $2) {	
	if($2.isNumber) {
		generateOutputNum(atoll($2.str), "A");
		pushOutputCommand("PUT", "A");
	} else {
		VariableStruct* var = getVariableByName($2.str);
			
		switch(var->type) {
			case ITERATOR: {
				if(!iteratorNamesStack.empty() && var->str == iteratorNamesStack.top()) {
					pushOutputCommand("PUT", "D");
				} else {
					generateOutputNum(var->memoryOffsetBegin, "A");
					pushOutputCommand("LOAD", "A");
					pushOutputCommand("PUT", "A");
				}
				break;
			}
			case VARIABLE: {
				if(var->isInitialized) {
					generateOutputNum(var->memoryOffsetBegin, "A");
					pushOutputCommand("LOAD", "A");
					pushOutputCommand("PUT", "A");
				} else {
					errorMessage("Variable '", $2.str, "' is not initialized");
				}
				break;
			}
			case ARRAY: {
				if($2.isConstantReference) {
					generateOutputNum(var->memoryOffsetBegin + atoll($2.reference) - var->start, "A");
					pushOutputCommand("LOAD", "A");
					pushOutputCommand("PUT", "A");
				} else if($2.isVariableReference) {
					VariableStruct* referencedVariable = getVariableByName($2.reference);

					if(referencedVariable->type == ITERATOR && !iteratorNamesStack.empty() && referencedVariable->str == iteratorNamesStack.top()) {
						pushOutputCommand("COPY", "B", "D");
					} else {
						generateOutputNum(referencedVariable->memoryOffsetBegin, "A");
						pushOutputCommand("LOAD", "B");
					}

					if(var->memoryOffsetBegin > var->start) {		
						generateOutputNum(var->memoryOffsetBegin - var->start, "A");
						pushOutputCommand("ADD", "A", "B");
					} else if (var->memoryOffsetBegin < var->start) {
						generateOutputNum(var->start, "A");
						pushOutputCommand("SUB", "B", "A");
						generateOutputNum(var->memoryOffsetBegin, "A");
						pushOutputCommand("SUB", "B", "A");
						pushOutputCommand("COPY", "A", "B");
					} else {
						pushOutputCommand("COPY", "A", "B");
					}
					pushOutputCommand("LOAD", "A");
					pushOutputCommand("PUT", "A");
				}
				break;
			}
			default: {
				errorMessage("Unexpected type '", &(var->type), "' to assign");
			}
		}
	}
}

void handleReadOperation(VariableStruct& $2) {
	VariableStruct* var = getVariableByName($2.str);
	
	switch(var->type) {
		case VARIABLE: {
			generateOutputNum(var->memoryOffsetBegin, "A");
			pushOutputCommand("GET", "B");
			pushOutputCommand("STORE", "B");
			initializeVariable(var);
			break;
		}
		case ITERATOR: {
			errorMessage("It's not allowed to READ into iterator '", $2.str, "'");
			break;
		}
		case ARRAY: {
			if($2.isConstantReference) {
				generateOutputNum(var->memoryOffsetBegin + atoll($2.reference) - var->start, "A");
				pushOutputCommand("GET", "B");
				pushOutputCommand("STORE", "B");
			} else if($2.isVariableReference) {
				VariableStruct* referencedVariable = getVariableByName($2.reference);

				if(referencedVariable->type == ITERATOR && !iteratorNamesStack.empty() && referencedVariable->str == iteratorNamesStack.top()) {
					pushOutputCommand("COPY", "B", "D");
				} else {
					generateOutputNum(referencedVariable->memoryOffsetBegin, "A");
					pushOutputCommand("LOAD", "B");
				}

				if(var->memoryOffsetBegin > var->start) {		
					generateOutputNum(var->memoryOffsetBegin - var->start, "A");
					pushOutputCommand("ADD", "A", "B");
				} else if (var->memoryOffsetBegin < var->start) {	
					generateOutputNum(var->start, "A");
					pushOutputCommand("SUB", "B", "A");
					generateOutputNum(var->memoryOffsetBegin, "A");
					pushOutputCommand("SUB", "B", "A");
					pushOutputCommand("COPY", "A", "B");
				} else {
					pushOutputCommand("COPY", "A", "B");
				}
				pushOutputCommand("GET", "B");
				pushOutputCommand("STORE", "B");
			}
			break;
		}
		default: {
			errorMessage("Unexpected type '", &(var->type), "' to assign");
		}
	}
}

void handlePrimitiveIdentifier(VariableStruct& varStruct, char* $1) {
	varStruct.str = $1;
	varStruct.type = VARIABLE;
	
	VariableStruct* var = getVariableByName($1);
	
	if(var == nullptr) {
		errorMessage("Undeclared variable '", $1, "' has been used");
	} else if(var->type == ARRAY) {
		errorMessage("Array '", $1, "' has been used as variable or iterator");
	}
}

void handleArrayReferenceWithIdentifier(VariableStruct& varStruct, char* $1, char* $3) {
	VariableStruct* var1 = getVariableByName($1);
	VariableStruct* var3 = getVariableByName($3);

	if(var1 == nullptr) {
		errorMessage("Undeclared variable '", $1, "' used as an array reference");
	} else if(var1->type != ARRAY) {
		errorMessage("Variable '", $1, "'is not an array");
	}
	
	if(var3 != nullptr) {
		if(var3->isInitialized) {			
			varStruct.isConstantReference = false;
			varStruct.isVariableReference = true;
			varStruct.reference = $3;
		} else {
			errorMessage("Variable '", $3, "' is not initialized");
		}
	} else {
		errorMessage("Undeclared variable '", $3, "' used as an array index");
	}
}

void handleArrayReferenceWithNum(VariableStruct& varStruct, char* $1, char* $3) {
	VariableStruct* var = getVariableByName($1);
	
	if(var != nullptr) {
		if(var->type != ARRAY) {
			errorMessage("Variable '", $1, "' is not an array");
		}		
		unsigned long long numValue = atoll($3);
		
		if(numValue < var->start || numValue > var->end) {
				std::string str;
				str = "Index ";
				str += $3;
				str += " of array '";
				errorMessage(str.c_str(), $1, "' out of table scope");
		}
		
		varStruct.isConstantReference = true;
		varStruct.isVariableReference = false;
		varStruct.reference = $3;
		varStruct.index = numValue;
	} else {
		errorMessage("Undeclared array '", $1, "' has been used");
	}
}

VariableStruct* getVariableByName(char* variableName) {
	std::string name(variableName);
	symbolsMapIterator = symbolsMap.find(name);
	
	if(symbolsMapIterator != symbolsMap.end()) {
		return &(symbolsMapIterator->second);
	}
	
	return nullptr;
}

void initializeVariableByName(char* variableName) {
	VariableStruct* var = getVariableByName(variableName);
	
	if(var != nullptr) {
		var->isInitialized = true;
	} else {
		errorMessage("Initialization of not declared variable '", variableName, "' is forbidden");
	}
}

void initializeVariable(VariableStruct* var) {
	var->isInitialized = true;
}

char* convertStringToCharPointer(std::string str) {
	return strdup(str.c_str());
}

void pushOutputCommand(std::string opCode) {
	pushOutputCommand(opCode, "", "");
}

void pushOutputCommand(std::string opCode, std::string arg1) {
	pushOutputCommand(opCode, arg1, "");
}

void pushOutputCommand(std::string opCode, unsigned long long arg1) {
	pushOutputCommand(opCode, std::to_string(arg1), "");
}

void pushOutputCommand(std::string opCode, std::string arg1, unsigned long long arg2) {
	pushOutputCommand(opCode, arg1, std::to_string(arg2));
}

void pushOutputCommand(std::string opCode, std::string arg1, std::string arg2) {
	opCode += " ";
	opCode += arg1;
	opCode += " ";
	opCode += arg2;
	outputCommands.push_back(opCode);
	outputCommandsSize++;
}

void generateOutputNum(unsigned long long num, std::string reg) {
	if (num < 2) {
		pushOutputCommand("SUB", reg, reg);
		
		if(num == 1) {
			pushOutputCommand("INC", reg);
		}
		
	} else {
		std::deque<int> binaryRepresentation;
		
		while (num != 0) {
			binaryRepresentation.push_front(num % 2);
			num = num / 2;
		}
		pushOutputCommand("SUB", reg, reg);
		pushOutputCommand("INC", reg);
		
		for (unsigned long long i = 1; i < binaryRepresentation.size(); i++) {
			if(binaryRepresentation[i] == 1) {
				pushOutputCommand("ADD", reg, reg);
				pushOutputCommand("INC", reg);
			} else if(binaryRepresentation[i] == 0) {
				pushOutputCommand("ADD", reg, reg);
			}
		}
	}
}

void generateNegation(std::string reg) {
	pushOutputCommand("JZERO", reg, outputCommandsSize+3);
	pushOutputCommand("SUB", "H", "H");
	pushOutputCommand("JUMP", outputCommandsSize+2);
	pushOutputCommand("INC", "H");
}

void generateNegation(std::string reg, std::string reg2) {
	pushOutputCommand("JZERO", reg, outputCommandsSize+3);
	pushOutputCommand("SUB", reg2, reg2);
	pushOutputCommand("JUMP", outputCommandsSize+2);
	pushOutputCommand("INC", reg2);
}

void storePreviousLoopValues(char* name) {
	VariableStruct* var = getVariableByName(name);
	generateOutputNum(var->memoryOffsetBegin, "A");
	pushOutputCommand("STORE", "D");
	pushOutputCommand("INC", "A");
	pushOutputCommand("STORE", "E");
}

void loadLoopValuesToRegistersByName(char* name) {
	VariableStruct* var = getVariableByName(name);
	generateOutputNum(var->memoryOffsetBegin, "A");
	pushOutputCommand("LOAD", "D");
	pushOutputCommand("INC", "A");
	pushOutputCommand("LOAD", "E");
}

unsigned long long loadVariableToRegisterWithCommandsSizeDiff(VariableStruct& varStruct, char* reg) {
	unsigned long long start = outputCommandsSize;
	loadVariableToRegister(varStruct, reg);
	unsigned long long end = outputCommandsSize;
	
	return (end - start);
}

void loadVariableToRegister(VariableStruct& varStruct, char* reg) {
	std::string regstr(reg);
	
	char* varName = varStruct.str;
	char* varReference = varStruct.reference;
	
	if(varStruct.isNumber) {
		generateOutputNum(atoll(varName), regstr);
	} else {
		VariableStruct* var = getVariableByName(varName);
		
		if(var != nullptr) {
			if(var->isInitialized) {
				if(var->type == ARRAY) {
					if(varStruct.isConstantReference) {
						generateOutputNum(var->memoryOffsetBegin + atoll(varReference) - var->start, "A");
						pushOutputCommand("LOAD", regstr);
					} else if(varStruct.isVariableReference) {
						VariableStruct* referencedVariable = getVariableByName(varReference);
						
						if(referencedVariable != nullptr) {
							if(referencedVariable->isInitialized) {
								if(referencedVariable->type == ITERATOR && !iteratorNamesStack.empty() && referencedVariable->str == iteratorNamesStack.top()) {
									pushOutputCommand("COPY", regstr, "D");
								} else {
									generateOutputNum(referencedVariable->memoryOffsetBegin, "A");
									pushOutputCommand("LOAD", regstr);
								}
									
								if(var->memoryOffsetBegin > var->start) {
									generateOutputNum(var->memoryOffsetBegin - var->start, "A");
									pushOutputCommand("ADD", "A", regstr);
								} else if (var->memoryOffsetBegin < var->start) {
									generateOutputNum(var->start, "A");
									pushOutputCommand("SUB", regstr, "A");
									generateOutputNum(var->memoryOffsetBegin, "A");
									pushOutputCommand("SUB", regstr, "A");
									pushOutputCommand("COPY", "A", regstr);
								} else {
									pushOutputCommand("COPY", "A", regstr);
								}
								pushOutputCommand("LOAD", regstr);
								
							} else {
								errorMessage("Variable '", varReference, "' used as an index is not initialized");
							}
						} else {
							errorMessage("Variable '", varReference, "' used as an index is not declared");
						}
					} else {
						errorMessage("Array '", varName, "' used without index");
					}
				} else if(var->type == ITERATOR) {
					if(!iteratorNamesStack.empty() && var->str == iteratorNamesStack.top()) {
						if(regstr != "D") {
							pushOutputCommand("COPY", regstr, "D");
						}
					} else {
						generateOutputNum(var->memoryOffsetBegin, "A");
						pushOutputCommand("LOAD", regstr);
					}
				} else {
					generateOutputNum(var->memoryOffsetBegin, "A");
					pushOutputCommand("LOAD", regstr);
				}
			} else {
				errorMessage("Variable '", varName, "' is not initialized");
			}
		} else {
			errorMessage("Variable '", varName, "' is not declared");
		}
	}
}

void printOutputCommands(char* outputFile) {
	std::ofstream outputStream;
	outputStream.open(outputFile);
	
	for(int i = 0; i < outputCommands.size(); i++) {
		outputStream << outputCommands.at(i) << "\n";
	}
	
	outputStream.close();

	printf("\033[0;32m\n ~~-Compilation successful !-~~\n\033[0m");
}

void errorMessage(std::string messageBegin, char* variableName, std::string messageEnd) {
	std::string name(variableName);
	errorMessage(messageBegin, name, messageEnd);
}

void errorMessage(std::string messageBegin, std::string variableName, std::string messageEnd) {
	messageBegin += variableName;
	messageBegin += messageEnd;
	yyerror(messageBegin.c_str());
}

void yyerror(const char* errorMessage) {
	printf("\033[0;31m\n ~~-Compilation failed !-~~\n\033[0m");
	printf("\033[0;31m[ ERROR ]: \033[0m%s on line %d\n", errorMessage, yylineno);
	exit(1);
}

int main(int argc, char** argv) {

	if(argc < 3) {
		printf(" Usage: kompilator \033[0;32m<input_file> \033[0;31m<output_file>\033[0m\n");
	} else {
		yyin = fopen(argv[1], "r");
		
		if(yyin == NULL) {
			yyerror("Input file not found..");
			exit(2);
		}

		yyparse();
		printOutputCommands(argv[2]);
		fclose(yyin);
	}
	
	return 0;
}
