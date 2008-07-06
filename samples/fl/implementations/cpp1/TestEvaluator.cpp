#include "language.h"
#include "CFunctionStorage.h"
#include "CEvaluator.h"
#include "COptimizer.h"
#include <fstream>
#include <stdlib.h>
#include "ParserError.h"
#include "EvaluationError.h"
using std::fstream;
// prototype of bison-generated parser function
int yyparse(CFunctionStorage*, CExpr*&);

extern FILE *yyin;
int main(int argc, char **argv)
{
	if(argc<4)
	{
		cerr<< "Error: missing filename for input or output file or missing expected result."<<endl;
		exit(1);
	}
	if(argc>4)
	{
		cerr<< "Error: Too many arguments."<<endl;
		exit(1);
	}
	
	if((yyin=fopen(argv[1],"r"))==NULL) 
	{
		cerr << "Error while opening file "<<argv[1]<<endl;
		exit(1);
	}
	CEvaluator* eval = new CEvaluator();
	CExpr* e;
	yyparse(eval,e);
	fclose(yyin);
	if((yyin=fopen(argv[2],"r"))==NULL) 
	{
		cerr << "Error while opening file "<<argv[2]<<endl;
		exit(1);
	}
	yyparse(0,e);
	fclose(yyin);
	
/*	CExpr* e2;
	COptimizer *opt=new COptimizer();
	e->accept(opt);
	delete e;
	e=opt->getResult();*/
	try {
		
		e->accept(eval);
		if(eval->result!=atoi(argv[3]))
		{
			cerr << "Error: result "<<eval->result<< " does not match expected result "<< argv[3] << endl;
		}	
		else
		{
			cout << "Success: result "<<eval->result<< " matches expected result "<< argv[3] << endl;
		}
	}
	catch(EvaluationError& e) 
	{
		cerr << e.what()<<endl;
	}
	
	delete eval;
	delete e;
	  //yyparse();
	return 0;
}

