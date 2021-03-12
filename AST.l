%{
	#include "header.h"
	#include <stdio.h>
	#include <ctype.h>
	#include <stdarg.h>
	#include <stdlib.h>
	#include "y.tab.h"
	#include<string.h>
	char st[100][10];
	int top=0;
	char i_[2]="0";
	char temp[2]="t",s[2],l[4];
	int label[20];
	int lnum=0;
	int ltop=0,cnt=0;
	int param_count=0;

	typedef struct quad{
		char op[10];
		char arg1[4];
		char arg2[4];
		char res[4];
		int dead;
	}q;

	q qple[500];

	//#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno;

  /* returns type from symbol table */

	#define sym_type(identifier) IDENTIFIER /* with no symbol table, fake it */

	int line = 0;

%}

%option yylineno

D   		[0-9]
L   		[a-zA-Z_]
A   		[a-zA-Z_0-9]
WS  		[ \t\v\n\f]


%%
"/*"		{ comment(); }
"//"[^\n]*	{ /* Consume Comment */ }

	/* Data Types */
int			{ strcpy(yylval.string,yytext); return(INT); }
float		{ strcpy(yylval.string,yytext); return(FLOAT); }
char		{ strcpy(yylval.string,yytext); return(CHAR); }
void		{ strcpy(yylval.string,yytext); return(VOID); }
main		{ strcpy(yylval.string,yytext); return(MAIN); }

	/* User Defined Data Types */
struct		{ strcpy(yylval.string,yytext); return(STRUCT); }

	/* Headers */
"#"		{ return HASH; }
include		{ return INCLUDE; }
define		{ return DEFINE; }

	/* C Libraries */
"math.h"	{ return MATH; }
"stdio.h"	{ return STDIO; }
"stdlib.h"	{ return STDLIB; }
"string.h"	{ return STRING; }
"time.h"	{ return TIME; }

	/* Control Structures */
for		{ return(FOR); }
if {return (IF);}
do {return (DO);}
while {return (WHILE);}

	/* User Defined Data Types, Identifiers */
{L}{A}*						{	 strcpy(yylval.string,yytext); push(); return IDENTIFIER;}
{D}+							{ strcpy(yylval.string,yytext); return INTEGER_LITERAL;}
{D}+\.{D}+						{ strcpy(yylval.string,yytext); return FLOAT_LITERAL;}
\"{A}+(".h"|".c")\"				{return HEADER_LITERAL;}
L?\"(\\.|[^\\"])*\"	{ return(STRING_LITERAL); }

	/* Assignment Operators */
"+="		{return(ADD_ASSIGN); }
"-="		{return(SUB_ASSIGN); }


	/* Relational Operators */
"++"		{return(INC_OP); }
"--"		{return(DEC_OP); }
"<="		{ strcpy(yylval.string,yytext); return(LE_OP); }
">="		{ strcpy(yylval.string,yytext); return(GE_OP); }
"=="		{ strcpy(yylval.string,yytext); return(EQ_OP); }
"!="		{ strcpy(yylval.string,yytext); return(NE_OP); }

	/* Basic Syntax */
";"		{return(';'); }
"{"	{return('{'); }
"}"	{return('}'); }
","		{return(','); }
":"		{return(':'); }
"="		{return('='); }
"("		{return('('); }
")"		{return(')'); }
("["|"<:")	{return('['); }
("]"|":>")	{return(']'); }
"."		{return('.'); }
"&"		{return('&'); }
"!"		{return('!'); }
"~"		{return('~'); }
"-"		{ strcpy(yylval.string,yytext);return('-'); }
"+"		{ strcpy(yylval.string,yytext);return('+'); }
"*"		{ strcpy(yylval.string,yytext);return('*'); }
"/"		{ strcpy(yylval.string,yytext);return('/'); }
"%"		{ strcpy(yylval.string,yytext);return('%'); }
"<"		{ strcpy(yylval.string,yytext);return('<'); }
">"		{ strcpy(yylval.string,yytext);return('>'); }
"^"		{return('^'); }
"|"		{return('|'); }
"?"		{return('?'); }


{WS}+		{ /* whitespace separates tokens */}
	
.		{ printf("No Match, Invalid Expression %s\n", yytext); }
%%

int yywrap(void)
{
	return 1;
}


static void comment(void)
{
    int c;

    while ((c = input()) != 0)
        if (c == '*')
        {
            while ((c = input()) == '*')
                ;

            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    yyerror("unterminated comment");
}

void yyerror(const char *str)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", str);
}

int main(int argc,char *argv[]){
	yyin = fopen(argv[1],"r");
	yyout = fopen("a.cpp","w");
	if(!yyparse())
	{
		printf("Successful\n");
	}
	else
		printf("Unsuccessful\n");
	opt();
	code_display();
	fclose(yyin);
	fclose(yyout);
	return 0;
}

nodeType *con(char *value)
{
	nodeType *p;
	/* allocate node */
	if ((p = malloc(sizeof(nodeType))) == NULL)
		yyerror("out of memory");
	/* copy information */
	p->type = typeCon;
	strcpy(p->con.value, value);
	return p;
}

nodeType *id(char *identifier) {
	nodeType *p;
	/* allocate node */
	if ((p = malloc(sizeof(nodeType))) == NULL)
		yyerror("out of memory");
	/* copy information */
	p->type = typeId;
	strcpy(p->id.name,identifier);
//	printf("The copied identifier %s\n", p->id.name);
	return p;
}

nodeType *opr(int oper, int nops, ...)
{
	va_list ap;
	nodeType *p;
	int i;
	/* allocate node, extending op array */
	if ((p = malloc(sizeof(nodeType) +(nops-1) * sizeof(nodeType *))) == NULL)
		yyerror("out of memory");
	/* copy information */
	p->type = typeOpr;
	p->opr.oper = oper;
	p->opr.nops = nops;
	va_start(ap, nops);
	for (i = 0; i < nops; i++)
		p->opr.op[i] = va_arg(ap, nodeType*);
	va_end(ap);
	return p;
}

void push()
{
strcpy(st[++top],yytext);
}

void codegen()
{
strcpy(temp,"t");
strcat(temp,i_);
//printf("%s = %s %s %s\n",temp,st[top-2],st[top-1],st[top]);
strcpy(qple[cnt].arg1,st[top-2]);
strcpy(qple[cnt].arg2,st[top]);
strcpy(qple[cnt].op,st[top-1]);
strcpy(qple[cnt++].res,temp);
top-=2;
strcpy(st[top],temp);
i_[0]++;
}


void codegen_umin()
{
strcpy(temp,"t");
strcat(temp,i_);
printf("%s = -%s\n",temp,st[top]);
top--;
strcpy(st[top],temp);
i_[0]++;
}


void codegen_assign()
{
//printf("%s = %s\n",st[top-2],st[top]);
strcpy(qple[cnt].arg1,st[top]);
strcpy(qple[cnt].res,st[top-2]);
strcpy(qple[cnt++].op,"=");
top-=2;
}

void lab1()
{
 strcpy(temp,"t");
 strcat(temp,i_);
 printf("%s = not %s\n",temp,st[top]);
 printf("if %s goto L%d\n",temp,lnum);
 i_[0]++;
}

void lab3()
{
printf("L%d: \n",lnum);
lnum++;
}

void class1(){
	printf("L%d: \n",lnum);
}

void class2(){
	i_[0]--;
	strcpy(temp,"t");
	strcat(temp,i_);
	printf("if %s goto L%d\n",temp,lnum++);
	i_[0]++;
}

void staf0(){
	//printf("L%d: \n",lnum);
	strcpy(qple[cnt].op,"label");
	sprintf(s,"%d",lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
}

void staf1(){
	i_[0]--;
	strcpy(temp,"t");
	strcat(temp,i_);
	//printf("if %s goto L%d\n",temp,++lnum);
	strcpy(qple[cnt].op,"if");
	strcpy(qple[cnt].arg1,temp);
	sprintf(s,"%d",++lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	//printf("if false goto L%d\n",lnum+=2);
	strcpy(qple[cnt].op,"ifFalse");
	strcpy(qple[cnt].arg1,temp);
	sprintf(s,"%d",lnum+=2);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	//printf("L%d: \n",--lnum);
	strcpy(qple[cnt].op,"label");
	sprintf(s,"%d",--lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	i_[0]++;

}

void staf2(){
	//printf("goto L%d: \n",lnum-=2);
	strcpy(qple[cnt].op,"goto");
	sprintf(s,"%d",lnum-=2);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	//printf("L%d: \n",++lnum);
	strcpy(qple[cnt].op,"label");
	sprintf(s,"%d",++lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
}

void staf3(){
	//printf("goto L%d: \n",++lnum);
	strcpy(qple[cnt].op,"goto");
	sprintf(s,"%d",++lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	//printf("L%d: \n",++lnum);
	strcpy(qple[cnt].op,"label");
	sprintf(s,"%d",++lnum);
	strcpy(l,"L");
	strcat(l,s);
	strcpy(qple[cnt++].res,l);
	strcpy(l,"");
	++lnum;
}

void floor1()
{
 lnum++;
 strcpy(temp,"t");
 strcat(temp,i_);
 printf("%s = not %s\n",temp,st[top]);
 printf("if %s goto L%d\n",temp,lnum);
 i_[0]++;
 label[++ltop]=lnum;
}

void floor2()
{
int x;
lnum++;
x=label[ltop--];
printf("goto L%d\n",lnum);
printf("L%d: \n",x);
label[++ltop]=lnum;
}

void floor3()
{
int y;
y=label[ltop--];
printf("L%d: \n",y);
ltop=0;
}

void funcinc()
{
	strcpy(temp,"t");
 	strcat(temp,i_);
	printf("%s = %s\n",temp,st[top]);
	printf("%s = %s + 1\n",st[top],st[top]);
	top--;
	i_[0]++;
}

void funcdec()
{
	strcpy(temp,"t");
 	strcat(temp,i_);
	//printf("%s = %s\n",temp,st[top]);
	printf("%s = %s - 1\n",st[top],st[top]);
	top--;
	i_[0]++;
}

void incfunc()
{
	strcpy(temp,"t");
 	strcat(temp,i_);
	printf("%s = %s + 1\n",st[top],st[top]);
	top--;
	i_[0]++;
}

void decfunc()
{
	strcpy(temp,"t");
 	strcat(temp,i_);
	printf("%s = %s - 1\n",st[top],st[top]);
	top--;
	i_[0]++;
}

void func()
{
	printf("param %s\n",st[top]);
	top--;
	param_count++;
}

void call()
{
	printf("call(%s,%d)\n",st[top],param_count);
	top--;
	param_count=0;
}

void opt(){
	int f=1;
	for(int i=0;i<500;i++){
		if(!strcmp(qple[i].op,"=") && strcmp(qple[i-1].arg2,"")){
			strcpy(qple[i-1].res,qple[i].res);
			qple[i].dead = 1;
		}
	}
	for(int i=0;i<500;i++){
		if(strcmp(qple[i].arg1,"") && strcmp(qple[i].arg2,"")){
			for(int j = i+1;j<500;j++){
				if(strcmp(qple[j].arg1,"") && strcmp(qple[j].arg2,"")){
					if(!strcmp(qple[j].arg1,qple[i].arg1) && !strcmp(qple[j].arg2,qple[i].arg2) && f==1){
						qple[j].dead = 1;
						for(int k = j+1;k<500;k++){
							if(!strcmp(qple[k].arg1,qple[j].res))
								strcpy(qple[k].arg1,qple[i].res);
							if(!strcmp(qple[k].arg2,qple[j].res))
								strcpy(qple[k].arg2,qple[i].res);
						}
					}
					else if(!strcmp(qple[j].res,qple[i].arg1) || !strcmp(qple[j].res,qple[i].arg2))
						f=0;
				}
				else if(!strcmp(qple[j].res,qple[i].arg1) || !strcmp(qple[j].res,qple[i].arg2))
						f=0;
			}
		}
	}
	int f1=0,i=0;
	q t;
	for(i=0;i<500;i++){
		if(!strcmp(qple[i].op,"goto"))
			break;
	}
	i++;
	int k;
	while(strcmp(qple[i].op,"goto")){
		if(strcmp(qple[i].arg2,"")){
			int j=i+1;
			while(strcmp(qple[j++].op,"goto")){
				if(!strcmp(qple[j].res,qple[i].arg1) || !strcmp(qple[j].res,qple[i].arg2)){
					return;
				}
			}
		}
		else if(!strcmp(qple[i].op,"label"))
			f1=1;
		k=i-1;
		if(!f1)
		{
			t = qple[i];
			while(strcmp(qple[k].res,"L0")){
				qple[k+1] = qple[k];
				k--;
			}
			qple[k+1] = qple[k];
			k--;
			while(strcmp(qple[k].res,"L0")){
				qple[k+1] = qple[k];
				k--;
			}
			qple[k+1] = qple[k];
			qple[k] = t;
		}
		f1=0;
		i++;
	}
}

void code_display(){
	for(int i = 0;i < 500;i++){
		if(!strcmp(qple[i].op,""))
			break;
		if(qple[i].dead!=1)
		{
			if(!strcmp(qple[i].op,"="))
				printf("%s %s %s\n",qple[i].res,qple[i].op,qple[i].arg1);
			else if(!strcmp(qple[i].op,"if") || !strcmp(qple[i].op,"ifFalse"))
				printf("%s %s %s\n",qple[i].op,qple[i].arg1,qple[i].res);
			else if(!strcmp(qple[i].op,"goto"))
				printf("%s %s\n",qple[i].op,qple[i].res);
			else if(!strcmp(qple[i].op,"label"))
				printf("%s:\n",qple[i].res);
			else
				printf("%s = %s %s %s\n",qple[i].res,qple[i].arg1,qple[i].op,qple[i].arg2);
		}
	}
}