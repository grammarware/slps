/* Mini Calculator */
/* calc.y */

%{
#include "language.h"
#include "CEvaluator.h"
#include "CFunctionStorage.h"
#include "ParserError.h"
int yyerror(CFunctionStorage* fs, CExpr*& expr, char *s);
int yylex(void);

%}

%union{
  int			int_val;
  string*		str_val;
  char			op_val;
  CExpr*		expr;
  CLiteral* 		lit;
  CFunction*		func;
  CArgument*		arg;
  std::vector<int>* 	vec;
  std::vector<CArgument*>*	arg_vec;
  std::vector<CExpr*>*	expr_vec;
}

%start	input 

%token	<int_val>	T_INTEGER_LITERAL
%type	<expr>	exp
%type	<expr>	atom
%type	<expr>	apply
%type	<expr>	binary
%type	<expr>	ifthenelse
%type   <func> definition
%type 	<arg_vec> arguments
%type 	<expr_vec> expr_list
//%type 	<arg> 	argument
%token   <str_val> T_ID
%type 	<func> definitions;
%left   T_PLUS T_MINUS T_EQUALS
%left	T_MULT
%left   T_LBRACKET
%right  T_RBRACKET
%left 	T_IF T_THEN T_ELSE
%left T_ASSIGN

%parse-param{ CFunctionStorage* fs}
%parse-param{ CExpr*& expr}

%%

input:		/* empty */
		| definitions {if(!fs) {cerr<<"No place to store definiton...\n";YYABORT;}}//{ if(!fs) yyerror(0,"calc.y: Definition found, but no FunctionStorage passed")}
		| exp	{expr = $1;}
		| definitions exp { expr= $2;}  
		;

definitions:	definition { fs->addFunction($1); $$=$1; }
		| definitions definition {$$=$1;fs->addFunction($2);}
		;
		
definition:	T_ID arguments T_ASSIGN exp { $$ = new CFunction((*$1),$2,$4);delete $1;}
		;

arguments:	T_ID {$$ = new vector<CArgument*>; $$->push_back(new CArgument((*$1)));delete $1;}
		| arguments T_ID {$$=$1; $1->push_back(new CArgument(*$2));delete $2;}
		;
		
			
exp:		binary {$$=$1;}
		| atom {$$=$1;}
		| ifthenelse { $$ = $1; }
		| apply {$$=$1;}
		;
	
binary:		T_MINUS T_LBRACKET exp T_RBRACKET { $$ = new CBinary(new CLiteral(0),$3,MINUS);}
		| exp T_PLUS exp	{ $$ = new CBinary($1,$3,PLUS); }
		| exp T_MINUS exp	{ $$ = new CBinary($1,$3,MINUS); }
		| exp T_EQUALS exp	{ $$ = new CBinary($1,$3,EQUALS); }
		;

atom:		T_ID {$$=new CArgument(*$1); delete $1; }
		| T_INTEGER_LITERAL	{ $$ = new CLiteral($1); }
		| T_LBRACKET exp T_RBRACKET {$$=$2}
		;

ifthenelse:	T_IF atom T_THEN atom T_ELSE atom { $$ = new CIfThenElse($2,$4,$6);}
				
apply:		T_ID expr_list { $$ = new CApply(*$1, $2); delete $1;}

expr_list:	atom {$$ = new vector<CExpr*>; $$->push_back($1);}
		| expr_list atom {$$=$1; $1->push_back($2);}
		;
%%

int yyerror(CFunctionStorage* fs, CExpr*& expr, string s)
{
  extern int yylineno;	// defined and maintained in lex.c
  extern char *yytext;	// defined and maintained in lex.c
  
  cerr << "ERROR: " << s << " at symbol \"" << yytext;
  cerr << "\" on line " << yylineno << endl;
//  exit(1);
throw ParserError(s);
return 1;
}




int yyerror(CFunctionStorage* fs, CExpr*& expr, char *s)
{
	return yyerror(fs, expr, string(s));
}


