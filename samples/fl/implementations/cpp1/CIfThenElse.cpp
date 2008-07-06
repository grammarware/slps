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