/* Mini Calculator */
/* calc.lex */

%{
#include "language.h"
#include "FL.tab.h"
#include "CFunctionStorage.h"
int yyerror(CFunctionStorage*, CExpr*&, char *s);
//int yylineno = 1;
%}

digit		[0-9]
int_const	{digit}+
id 		[a-zA-Z][a-zA-Z0-9]*
%%

{int_const}	{ yylval.int_val = atoi(yytext); return T_INTEGER_LITERAL; }
"+"		{ yylval.op_val = yytext[0]; return T_PLUS; }
"*"		{ yylval.op_val = yytext[0]; return T_MULT; }
"-"		{ yylval.op_val = yytext[0]; return T_MINUS; }
"("		{ yylval.op_val = yytext[0]; return T_LBRACKET; }
")"		{ yylval.op_val = yytext[0]; return T_RBRACKET; }
"=="		{ yylval.op_val = yytext[0]; return T_EQUALS; }
"="		{ yylval.op_val = yytext[0]; return T_ASSIGN; }
"if"		{ yylval.op_val = yytext[0]; return T_IF; }
"then"		{ yylval.op_val = yytext[0]; return T_THEN; }
"else"		{ yylval.op_val = yytext[0]; return T_ELSE; }

{id}		{ yylval.str_val= new string(yytext); return T_ID; }
[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ std::cerr << "SCANNER "; CExpr* e=0;yyerror(0,e,0); exit(1);	}

