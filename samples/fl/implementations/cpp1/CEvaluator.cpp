/***************************************************************************
 *   Copyright (C) 2008 by Andreas Mützel   *
 *   amuetzel@uni-koblenz.de   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include "CEvaluator.h"
#include "EvaluationError.h"
CEvaluator::CEvaluator()
		: CVisitor()
{
}


CEvaluator::~CEvaluator()
{
}


void CEvaluator::visit ( CApply* e )
{
	CFunction *f= getFunction(e->getFunction());
	if(!f) {
		throw(EvaluationError("function \""+e->getFunction()+"\" is not defined."));
	}
	if(!f || (f->getNumArguments() != e->getNumArguments())) {
		throw(EvaluationError("number of arguments for function \""+e->getFunction()+"\" does not match declaration."));
	}
	map<string,int> *newContext= new map<string,int>();
	for(int i=0; i<f->getNumArguments();i++) {
		string tmp=f->getArgument(i)->getName();
		e->getArgument(i)->accept(this);
		int value=result;
	//	cout << "  "<< tmp <<"="<<value<<endl;
		(*newContext)[tmp]=value;
	}
	pushContext(newContext);
	CExpr *e2 = f->getExpression();
	e2->accept(this);
	delete popContext();
	//return result;
}

void CEvaluator::visit ( CArgument* e )
{
	result=getArgumentValue(e->getName());
}

void CEvaluator::visit ( CBinary* e )
{
	e->getLeft()->accept(this);
	int a=result;
	e->getRight()->accept(this);
	int b=result;
	switch(e->getType())
	{
		case PLUS:
			result=a+b;
			break;
		case MINUS:
			result=a-b;
			break;
		case EQUALS:
			if(a==b)
				result=1;
			else
				result=0;
			break;
	}
}

void CEvaluator::visit ( CExpr* e )
{
	cout <<"Error: CExpr visited"<<endl;
}

void CEvaluator::visit ( CIfThenElse* e )
{
	e->getCondition()->accept(this);
	if(result!=0) 
		e->getIfPart()->accept(this);
	else
		e->getElsePart()->accept(this);
}

void CEvaluator::visit ( CLiteral* e )
{
	result=e->getValue();
}

void CEvaluator::visit ( CFunction* e )
{
	throw EvaluationError("tried to visit function"+e->getName());
}

void CEvaluator::pushContext(map<string,int> *context) 
{
	m_vContext.push_back(context);
}
map<string,int>* CEvaluator::popContext() 
{
	map<string,int> *tmp = m_vContext.back();
	m_vContext.pop_back();
	return tmp;
}
map<string,int>* CEvaluator::getContext() 
{
	map<string,int> *tmp = m_vContext.back();
	return tmp;
}

int CEvaluator::getArgumentValue(const string& name){
	if(m_vContext.size()>0) 
	{
		if(m_vContext.back()->find(name)==m_vContext.back()->end()){
			throw(EvaluationError("argument \""+name+"\" does not have an associated value."));
		}
		return (*m_vContext.back())[name];
	}
	throw(EvaluationError("getArgumentValue(): no argument defined."));
}
