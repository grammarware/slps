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
#ifndef _CVISITOR_HEADER_INCLUDED_
#define _CVISITOR_HEADER_INCLUDED_
//#include "language.h"
class CExpr;
class CApply;
class CArgument;
class CBinary;
class CIfThenElse;
class CLiteral;
class CFunction;

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Base class for all visitors.
	
	Base class for visitors that can visit all CExpr-objects and also objects of class CFunction.
 */
class CVisitor {
	public: 
		virtual void visit(CExpr* e)=0;
		virtual void visit(CApply* e)=0;
		virtual void visit(CArgument* e)=0;
		virtual void visit(CBinary* e)=0;
		virtual void visit(CIfThenElse* e)=0;
		virtual void visit(CLiteral* e)=0;
		virtual void visit(CFunction* e)=0;
};


#endif