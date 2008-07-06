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
