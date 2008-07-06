#include "CApply.h"

CApply::CApply(string name,vector<CExpr*> * arguments)
 : CExpr()
{
//	cout << "created CApply"<<endl;
	m_sFunction=name;
	if(arguments)
		m_eArguments=arguments;
	else
		m_eArguments = new vector<CExpr*>();
}
CApply::~CApply()
{
	if(m_eArguments)
	{
		while (m_eArguments->size()>0)
		{
			delete m_eArguments->back();
			m_eArguments->pop_back();
		}
	}
	delete m_eArguments;
	
}


CExpr* CApply::clone()
{
	CApply *result=new CApply(m_sFunction);
	for(int i=0;i<m_eArguments->size();i++)
	{
		result->addArgument(getArgument(i)->clone());
	}
	return result;
}

void CApply::accept(CVisitor* v)
{
	if(v) v->visit(this);
}

void CApply::addArgument(CExpr* e) {
	m_eArguments->push_back(e);
}

int CApply::getNumArguments() {
	return m_eArguments->size();
}
CExpr* CApply::getArgument(unsigned int i) {
	if(i>=m_eArguments->size()) {
		return 0; //ERROR
	}
	return m_eArguments->at(i);
}

string CApply::getFunction(){
	return m_sFunction;
}

bool CApply::operator== (CExpr& other)
{
	CApply* a= dynamic_cast<CApply*> (&other);
	if(a==0)
		return false;
	if(m_sFunction!=a->m_sFunction)
		return false;
	int n=getNumArguments();
	if(n!=a->getNumArguments())
		return false;
	for(int i=0; i<n; i++)
	{
		if(!(getArgument(i)==a->getArgument(i)))
			return false;
	}
	return true;
}