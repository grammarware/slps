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
#ifndef CAPPLY_H
#define CAPPLY_H
#include "language.h"
/*#include <string>
#include <vector>*/

using std::string;
using std::vector;
/**
	\brief This class represents function calls.
	
	It stores the name of the called function and the expressions, which are passed as arguments.
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CApply : public CExpr
{
	public:
		/**
		Constructor...
		@param name name of the function
		@param arguments a vector of CExpr-pointers, may be null if no arguments are needed
		*/
		CApply(string name, vector<CExpr*> * arguments=0);

		~CApply();
		virtual CExpr* clone();
		virtual void accept(CVisitor* v);
		CExpr *getArgument(unsigned int i);
		int getNumArguments();
		void addArgument(CExpr* e);
		string getFunction();
		virtual bool operator== (CExpr& other);
	private:
		string m_sFunction;
		vector<CExpr*> *m_eArguments;
};

#endif
