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
#ifndef CBinary_h
#define CBinary_h

#include "language.h"

enum BinaryOpType {
	EQUALS,
 PLUS,
 MINUS
};

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Represents binary Expressions.
	
	This Class represents 3 types of binary expressions: Plus, Minus and Equals	
 */
class CBinary : public CExpr
{
	public:
		/**
		Constructor...
		@param left the first sub-expression
		@param right the second sub-expression
		@param type BinaryOpType, describes which operation to perform 
		 */
		CBinary(CExpr* left, CExpr* right, BinaryOpType type);

		~CBinary();

		virtual void accept(CVisitor* v);
    
		virtual CExpr *clone();
	
		CExpr *getLeft(), *getRight();
		BinaryOpType getType();
		void setLeft(CExpr* e), setRight(CExpr* e);
		void setType(BinaryOpType type);
		virtual bool operator== (CExpr& other);
	
	private:
		CExpr* l;
		CExpr* r;	
		BinaryOpType t;
};

#endif
