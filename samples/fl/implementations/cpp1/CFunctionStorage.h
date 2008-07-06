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
#ifndef CFUNCTIONSTORAGE_H
#define CFUNCTIONSTORAGE_H
//#include "language.h"

#include "CFunction.h"
#include <vector>
using namespace std;
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CFunctionStorage
{
	public:
		CFunctionStorage();

		virtual ~CFunctionStorage();
		void addFunction(/*const string& name,*/ CFunction* function) ;
		CFunction* getFunction(const string& name);
		CFunction* getFunction(int number);
		int getNumFunctions();
		void deleteFunctions();
	private:
		vector <CFunction*> m_vFunctions;

};

#endif
