#include "CIfThenElse.h"

CIfThenElse::CIfThenElse(CExpr* condition, CExpr *ifPart, CExpr *elsePart)
		: CExpr()
{
	m_eCondition=condition;
	m_eIfPart=ifPart;
	m_eElsePart=elsePart;
}


CIfThenElse::~CIfThenElse()
{
	if(m_eCondition)
		delete m_eCondition;
	if(m_eIfPart)
		delete m_eIfPart;
	if(m_eElsePart)
		delete m_eElsePart;
}


CExpr* CIfThenElse::clone()
{
	return new CIfThenElse(m_eCondition->clone(), m_eIfPart->clone(), m_eElsePart->clone());
}

void CIfThenElse::accept ( CVisitor* v )
{
	if(v) v->visit(this);
}

CExpr* CIfThenElse::getCondition() 
{
	return m_eCondition;
}
CExpr* CIfThenElse::getIfPart()
{
	return m_eIfPart;
}
CExpr* CIfThenElse::getElsePart()
{
	return m_eElsePart;
}

bool CIfThenElse::operator== (CExpr& other)
{
	CIfThenElse* i= dynamic_cast<CIfThenElse*>(&other);
	if(i==0)
		return false;
	return (m_eCondition==i->m_eCondition)&&(m_eIfPart==i->m_eIfPart)&&(m_eElsePart==i->m_eElsePart);
}