//
// C++ Implementation: CPrettyPrinter
//
// Description:
//
//
// Author: Andreas Mützel <amuetzel@uni-koblenz.de>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "CPrettyPrinter.h"
#include "language.h"

CPrettyPrinter::CPrettyPrinter(ostream& stream)
		: CVisitor(), out(stream)
{
	//result="";
	//out=stream;
}


CPrettyPrinter::~CPrettyPrinter()
{
}

void CPrettyPrinter::visit(CExpr* e)
{
}
void CPrettyPrinter::visit(CApply* e)
{
	out << "(";
	out << e->getFunction();
	for(int i=0; i<e->getNumArguments(); i++) 
	{
		out <<" ";
		e->getArgument(i)->accept(this);
	}
	out << ")";
}
void CPrettyPrinter::visit(CArgument* e)
{
	out <<e->getName();
}
void CPrettyPrinter::visit(CBinary* e)
{
	/*result+="(";
	e->getLeft()->accept(this);
	switch (e->getType()) {
		case PLUS:
			result+="+";
			break;
		case MINUS:
			result+="-";
			break;
		case EQUALS:
			result+="==";
			break;
	}
	e->getRight()->accept(this);
	result+=")";*/
	out << "(";
	e->getLeft()->accept(this);
	switch (e->getType()) {
		case PLUS:
			out <<" + ";
			break;
		case MINUS:
			out <<" - ";
			break;
		case EQUALS:
			out <<"==";
			break;
	}
	e->getRight()->accept(this);
	out <<")";
}
void CPrettyPrinter::visit(CIfThenElse* e) 
{
	out << "if ";
	e->getCondition()->accept(this);
	out << " then ";
	e->getIfPart()->accept(this);
	out << " else ";
	e->getElsePart()->accept(this);
}
void CPrettyPrinter::visit(CLiteral* e) 
{
	//result+=e->getValue();
	out << e->getValue();
}

void CPrettyPrinter::visit(CFunction* f) 
{
	out << f->getName()<< " ";
	for(int i=0; i< f->getNumArguments(); i++) 
	{
		out <<f->getArgument(i)->getName()<< " ";
	}
	out << "= ";
	f->getExpression()->accept(this);
	out << endl;
}

