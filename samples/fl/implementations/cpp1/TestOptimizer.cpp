#include "language.h"
#include "CFunctionStorage.h"
#include "CEvaluator.h"
#include "COptimizer.h"
#include "CPrettyPrinter.h"
#include <fstream>
#include <stdlib.h>
using std::fstream;
// prototype of bison-generated parser function
int yyparse(CFunctionStorage*, CExpr*&);

extern FILE *yyin;
int main(int argc, char **argv)
{
	CEvaluator* eval = new CEvaluator();
	CPrettyPrinter *printer= new CPrettyPrinter(std::cout);
	CExpr* e;
	yyparse(eval,e);
	COptimizer *opt=new COptimizer();
	e->accept(opt);
	delete e;
	e=opt->getResult();
	e->accept(printer);
	delete eval;
	delete e;
	delete opt;
	delete printer;
	  //yyparse();
	cout << endl<<endl;
	return 0;
}

