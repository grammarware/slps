#include "CArgument.h"

CArgument::CArgument ( string name )
{
	m_sName=name;
}


CArgument::~CArgument()
{
}


CExpr* CArgument::clone()
{
	return new CArgument(m_sName);
}

void CArgument::accept ( CVisitor* v )
{
	if(v) v->visit(this);
}

bool CArgument::operator== (CExpr& other) 
{
	CArgument* a= dynamic_cast<CArgument*>(&other);
	if(a==0)
	{
		return false;
	}
	return m_sName==a->m_sName;
}
