%{
	#include <stdio.h>
	#include <string.h>
	#include<ctype.h>
	#include"header.h"
	int if_assign = 1;

%}

%union
{
	int ival;
	nodeType *nPtr;
	char string[128];
}

%token HASH INCLUDE DEFINE STDIO STDLIB MATH STRING TIME

%token	IDENTIFIER INTEGER_LITERAL FLOAT_LITERAL STRING_LITERAL HEADER_LITERAL

%token	INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP

%token	ADD_ASSIGN SUB_ASSIGN

%token	CHAR INT FLOAT VOID MAIN GET PUT WRITE CIN COUT GETLINE BREAK RETURN CONT MANIP

%token	STRUCT

%token	FOR WHILE DO IF

%type <string> IDENTIFIER INTEGER_LITERAL FLOAT_LITERAL STRING_LITERAL 

%type <nPtr> primary_expression postfix_expression multiplicative_expression

%type <nPtr> unary_expression additive_expression relational_expression

%type <nPtr> equality_expression conditional_expression assignment_expression

%type <nPtr> statement compound_statement expression_statement block_item

%type <nPtr> expression init_declarator init_declarator_list

%type <nPtr> iteration_statement block_item_list translation_unit

%type <nPtr> external_declaration declaration


%start translation_unit

%%
headers
	: HASH INCLUDE HEADER_LITERAL 
	| HASH INCLUDE '<' libraries '>'
	;

libraries
	: STDIO
	| STDLIB
	| MATH
	| STRING
	| TIME
	;

primary_expression
	: IDENTIFIER		{$$ = id($1);}
	| INTEGER_LITERAL{push();}	{$$ = con($1);}
	| FLOAT_LITERAL{push();}		{$$ = con($1);}
	| STRING_LITERAL{push();}	{$$ = con($1);}
	| '(' expression ')'	{$$ = $2;}
	;

postfix_expression
	: primary_expression	{$$ = $1;}
	| postfix_expression '(' ')'
	| postfix_expression '.' IDENTIFIER {
											char *tmp = strcat($1->id.name,".");
											tmp = strcat(tmp, $3);
											printf(" %s\n", tmp); 			
											$$ = id(tmp);
										}
	| postfix_expression INC_OP			{	funcinc();
											$$ = opr('=', 2, $1, opr('+', 2, $1, con("1") ) );
										}
	| postfix_expression DEC_OP			{	funcdec();
											$$ = opr('=', 2, $1, opr('-', 2, $1, con("1") ) );										
										}
	| INC_OP primary_expression			{	incfunc();
											$$ = opr('=', 2, $2, opr('+', 2, $2, con("1") ) );										
										}
	| DEC_OP primary_expression			{	decfunc();
											$$ = opr('=', 2, $2, opr('-', 2, $2, con("1") ) );
										}

	;

unary_expression
	: postfix_expression 			{$$ = $1;}
	| '+'{push();} unary_expression{codegen_umin(); $$ = opr('+', 1, $3);}
	| '-'{push();} unary_expression{codegen_umin(); $$ = opr('+', 1, $3);}	
	;

multiplicative_expression
	: unary_expression						{$$ = $1;}
	| multiplicative_expression '*'{push();} unary_expression{codegen(); $$ = opr('*', 2, $1, $4);}
	| multiplicative_expression '/'{push();} unary_expression{codegen(); $$ = opr('/', 2, $1, $4);}		
	| multiplicative_expression '%'{push();} unary_expression{codegen(); $$ = opr('%', 2, $1, $4);}
	;

additive_expression
	: multiplicative_expression					{$$ = $1;}
	| additive_expression '+'{push();} multiplicative_expression{codegen(); $$ = opr('+', 2, $1, $4);}
	| additive_expression '-'{push();} multiplicative_expression{codegen(); $$ = opr('-', 2, $1, $4);}
	;

relational_expression
	: additive_expression
	| relational_expression '<'{push();} additive_expression{codegen(); $$ = opr('<', 2, $1, $4);}
	| relational_expression '>'{push();} additive_expression{codegen(); $$ = opr('>', 2, $1, $4);}
	| relational_expression LE_OP{push();} additive_expression{codegen(); $$ = opr(LE_OP, 2, $1, $4);}
	| relational_expression GE_OP{push();} additive_expression{codegen(); $$ = opr(GE_OP, 2, $1, $4);}
	;

equality_expression
	: relational_expression						{$$ = $1;}
	| equality_expression EQ_OP{push();} relational_expression{codegen(); $$ = opr(EQ_OP, 2, $1, $4);}
	| equality_expression NE_OP{push();} relational_expression{codegen(); $$ = opr(NE_OP, 2, $1, $4);}
	;

conditional_expression
	: equality_expression						{$$ = $1;}
	| equality_expression {floor1();}'?' expression{floor2();} ':' conditional_expression{floor3(); $$ = opr('?', 2, $1, opr(':', 2, $4, $7) );}
	;

assignment_expression
	: conditional_expression					{$$ = $1;}
	| unary_expression '='{push();} assignment_expression{codegen_assign(); $$ = opr('=', 2, $1, $4);}
	| unary_expression ADD_ASSIGN{push();} assignment_expression{codegen(); $$ = opr('=', 2, $1, opr('+', 2, $1, $4) );}
	| unary_expression SUB_ASSIGN{push();} assignment_expression{codegen(); $$ = opr('=', 2, $1, opr('-', 2, $1, $4) );}
	;

expression
	: assignment_expression						{$$ = $1;}
	| expression ',' assignment_expression
	;

constant_expression
	: conditional_expression					{}
	;

declaration
	: type_specifier ';'								//{$$ = opr(';', 1, $1);}
	| type_specifier init_declarator_list ';'			{$$ = opr(';', 1, $2);}
	;

init_declarator_list
	: init_declarator									{$$ = $1;}
	| init_declarator_list ',' init_declarator			{$$ = opr(',', 2, $1, $3);}
	;

init_declarator
	: IDENTIFIER '='{push();} assignment_expression{codegen_assign(); $$ = opr('=', 2, id($1), $4);}
	| IDENTIFIER{ $$ = id($1);}
	;

type_specifier
	: VOID
	| CHAR
	| INT
	| FLOAT
	| struct_specifier				{if_assign = 0;}
	;

struct_specifier
	: STRUCT '{' struct_declaration_list '}'
	| STRUCT IDENTIFIER '{' struct_declaration_list '}'
	| STRUCT IDENTIFIER
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list ';'
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| IDENTIFIER ':' constant_expression
	| IDENTIFIER
	;

statement
	: compound_statement	{$$ = $1;}
	| expression_statement	{$$ = $1;}
	| iteration_statement	{$$ = $1;}
	;

compound_statement
	: '{' '}'
	| '{' block_item_list '}'	{$$ = $2;}
	;

block_item_list
	: block_item	{$$ = $1;}
	| block_item_list block_item {$$ = opr(';', 2, $1, $2);}
	;

block_item
	: declaration	{$$ = $1;}
	| statement		{$$ = $1;}
	;

expression_statement
	: ';'			
	| expression ';' {$$ = $1;}
	;

iteration_statement
	: FOR '(' expression_statement{staf0();} expression_statement{staf1();} expression{staf2();} ')' statement{staf3(); $$ = opr(FOR, 4, $3, $5, $7, $10);}
	| DO{class1();} statement  WHILE '(' expression ')'';'{class2(); $$ = opr(DO, 2, $3, $6);}
	| IF '(' expression ')'{lab1();} statement{lab3(); $$ = opr(IF, 2, $3, $6);}
	;

translation_unit
	: external_declaration
	| translation_unit external_declaration 	;

external_declaration
	: INT MAIN '(' ')' compound_statement	{ex($5, 0); /*freeNode($2);*/}
	| declaration							{
												if(if_assign)
												{
													ex($1, 2); /*freeNode($2);*/
												}
											}	
	| headers 	
	;

%%

