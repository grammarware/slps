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
#ifndef _CLITERAL_HEADER_INCLUDED_
#define _CLITERAL_HEADER_INCLUDED_
#include "language.h"


/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief The class for literals
 */

class CLiteral: public CExpr {
	public:
		CLiteral(int value);
		virtual void accept(CVisitor* v);
		virtual ~CLiteral(){};
		virtual CExpr* clone();
		int getValue();
		void setValue(int i);
		/*int operator() (void) {
			return v;
		};
		friend ostream& operator <<(ostream &os,const CLiteral &obj){
			os << obj.v;
			return os;
		};*/
		virtual bool operator== (CExpr& other);
	protected: 
		int v;
};
#endif
