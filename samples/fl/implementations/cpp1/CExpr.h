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
#ifndef _CEXPR_HEADER_INCLUDED_
#define _CEXPR_HEADER_INCLUDED_
#include "language.h"
#include <iostream>
using namespace std;
class CFunctionStorage;

/**
	@brief Base class for all expressions.
	*/

class CExpr {
	public: 
		/**
			accepts a visitor.
			@param v a CVisitor*
		*/
		virtual void accept(CVisitor* v)=0;
		virtual ~CExpr() {};
		/**
			Returns a pointer to a deep clone of the expression.	
		*/
		virtual CExpr* clone()=0;
		int operator() (CFunctionStorage* fs=0) {
			return 0; //TODO: implement or remove this...
		}
		/**
			Compares two expressions.
		*/
		virtual bool operator== (CExpr& other) {
			return false;
		}
		/**
			Prints an expression by using a temporary CPrettyPrinter-Visitor.
		*/
		friend ostream& operator<<(ostream &os, CExpr &obj);
	private:
};
#endif