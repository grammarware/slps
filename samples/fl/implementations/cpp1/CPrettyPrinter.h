#ifndef CPRETTYPRINTER_H
#define CPRETTYPRINTER_H

#include "CVisitor.h"
#include <string>
#include <ostream>
using std::string;
using std::ostream;

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Visitor that prints any expression.
	
	This visitor is used to print expressions. It prints to any ostream, for example cout, cerr, any stringstream or an ofstream.
 */
class CPrettyPrinter : public CVisitor
{
	public:
		/** 
			Constructor...
			@param stream A reference to any ostream-object. Output will be printed to that stream.
		*/
		CPrettyPrinter(ostream& stream);

		~CPrettyPrinter();
		
		virtual void visit(CExpr* e);
		virtual void visit(CApply* e);
		virtual void visit(CArgument* e);
		virtual void visit(CBinary* e);
		virtual void visit(CIfThenElse* e);
		virtual void visit(CLiteral* e);
		virtual void visit(CFunction* e);
	private: 
		ostream& out;
};

#endif
