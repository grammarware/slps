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
#ifndef CIFTHENELSE_H
#define CIFTHENELSE_H

#include "language.h"

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief "The If-Then-Else"-expression
	
*/
class CIfThenElse : public CExpr
{
	public:
		/**
		Constructor...
		@param condition The expression to be used as the condition, if it evaluates to 0 then the else-part will be evaluated, else the if-part.
		@param ifPart The IF-part
		@param elsePart The ELSE-part
		*/
		CIfThenElse(CExpr* condition, CExpr *ifPart, CExpr *elsePart);

		~CIfThenElse();

		virtual CExpr* clone();
		virtual void accept ( CVisitor* v );
		CExpr *getCondition();
		CExpr *getIfPart();
		CExpr *getElsePart();
		virtual bool operator== (CExpr& other);
	protected:
		CExpr *m_eCondition, *m_eIfPart, *m_eElsePart;
};

#endif
