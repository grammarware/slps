//
// C++ Implementation: COptimizer
//
// Description:
//
//
// Author: Andreas Mützel <amuetzel@uni-koblenz.de>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "COptimizer.h"

COptimizer::COptimizer()
{
	result=0;
}


COptimizer::~COptimizer()
{
}

CExpr* COptimizer::getResult()
{
	return result;
}

void COptimizer::visit ( CApply* e )
{
	result=e->clone();
}

void COptimizer::visit ( CArgument* e )
{
	result=e->clone();
}

void COptimizer::visit ( CBinary* e )
{
	e->getLeft()->accept(this);
	CExpr* newLeft=result;
	e->getRight()->accept(this);
	CExpr* newRight=result;
	
 // try to cast l1 and l2 to CLiteral and optimize
	CLiteral *l1=0;
	CLiteral *l2=0;
	l1=dynamic_cast<CLiteral*>(newLeft);
	l2=dynamic_cast<CLiteral*>(newRight);
	// l1 is a CLiteral, w/ value=0: return l1, ignore l2
	if(((e->getType()==PLUS)||(e->getType()==MINUS))&&(l1==0)&&(l2!=0)&&l2->getValue()==0) 
	{
		result=l1;
		delete l2;
		return;
	}
	// l2 is a CLiteral, w/ value=0: return l2, ignore l1
	if((e->getType()==PLUS)&&(l1!=0)&&(l2==0)&&l1->getValue()==0) 
	{
		result=l2;
		delete l1;
		return;
	}
	// l1 and l2 are CLiterals, calculate result and return a new CLiteral 
	if((l1!=0)&&(l2!=0)) 
	{
		switch(e->getType())
		{
			case PLUS:
				result=new CLiteral(l1->getValue()+l2->getValue());
				delete l1;
				delete l2;
				return;
			case MINUS:
				result=new CLiteral(l1->getValue()-l2->getValue());
				delete l1;
				delete l2;
				return;
			case EQUALS:
				result=new CLiteral((l1->getValue()==l2->getValue())?1:0);
				delete l1;
				delete l2;
				return;
		}
	}
	if(e->getType()==EQUALS&&(*e->getLeft()==*e->getRight()))
	{
		result = new CLiteral(1);
		delete l1;
		delete l2;
		return;
	}
	result=new CBinary(newLeft,newRight,e->getType());
}

void COptimizer::visit ( CExpr* e )
{
	cerr << "COpimizer.cpp..."<<endl;
}

void COptimizer::visit ( CFunction* e )
{
	cerr << "COpimizer.cpp..."<<endl;
}

void COptimizer::visit ( CIfThenElse* e )
{
	e->getCondition()->accept(this);
	CExpr* newCond=result;
	e->getIfPart()->accept(this);
	CExpr* newIf=result;
	e->getElsePart()->accept(this);
	CExpr* newElse=result;
	
	// the condition is simply a literal
	CLiteral* condLiteral = dynamic_cast<CLiteral*>(newCond);
	if(condLiteral)
	{
		if(condLiteral->getValue()==0)
		{
			delete condLiteral;
			delete newIf;
			result=newElse;
			return;
		}
		else
		{
			delete condLiteral;
			result= newIf;
			delete newElse;
			return;
		}
	}
	
	// "if" and "else"-parts are equal, delete the condition and the if-part, return the else-part.
	if(*newIf==*newElse)
	{
		delete newCond;
		delete newIf;
		result=newElse;
		return;
	}
	
	result=new CIfThenElse(newCond,newIf,newElse);
}

void COptimizer::visit ( CLiteral* e )
{
	result=e->clone();
}

