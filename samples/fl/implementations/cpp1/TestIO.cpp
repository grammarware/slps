#include "language.h"
#include "CFunctionStorage.h"
#include "CPrettyPrinter.h"
#include <fstream>
#include <stdlib.h>
using std::fstream;
// prototype of bison-generated parser function
int yyparse(CFunctionStorage*, CExpr*&);

extern FILE *yyin;
int main(int argc, char **argv)
{
	if(argc<3)
	{
		cerr<< "Error: missing filename for input or output file."<<endl;
		exit(1);
	}
	if(argc>3)
	{
		cerr<< "Error: Too many arguments."<<endl;
		exit(1);
	}
	
	if((yyin=fopen(argv[1],"r"))==NULL) 
	{
		cerr << "Error while opening file "<<argv[1]<<endl;
		exit(1);
	}
	
	CFunctionStorage fs;
	CExpr* e;
	yyparse(&fs,e);
	
	fstream stream(argv[2], ios_base::out);
	CPrettyPrinter * printer = new CPrettyPrinter(stream);
	for(int i=0; i< fs.getNumFunctions(); i++)
	{
		stream << *fs.getFunction(i);//->accept(printer);
	}
	
  //yyparse();
	return 0;
}

