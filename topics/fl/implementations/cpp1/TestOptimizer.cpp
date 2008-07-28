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
	CExpr* e;
	yyparse(eval,e);
	COptimizer *opt=new COptimizer();
	e->accept(opt);
	delete e;
	e=opt->getResult();
	if(e) 
	{
		cout << *e;
		delete e;
	}
	delete eval;
	delete opt;
	cout << endl<<endl;
	return 0;
}

