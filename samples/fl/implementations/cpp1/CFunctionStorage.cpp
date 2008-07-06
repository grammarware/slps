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
#include "CFunctionStorage.h"

CFunctionStorage::CFunctionStorage()
{
}


CFunctionStorage::~CFunctionStorage()
{
	deleteFunctions();
}

void CFunctionStorage::addFunction(/*const string& name, */CFunction* function) 
{
	for(int i=0; i<m_vFunctions.size();i++) {
		if(m_vFunctions[i]->getName()==function->getName()) {
//			cout << "found function "<< name <<endl;
			cerr << "Warnung: function "<<function->getName()<<" already exists, ignoring new definition"<<endl;  
			return;
		}
	}
//	cout << "adding function "<< function->getName()<<endl;
	m_vFunctions.push_back(function);
}

CFunction* CFunctionStorage::getFunction(const string& name)
{
	for(int i=0; i<m_vFunctions.size();i++) {
		if(m_vFunctions[i]->getName()==name) {
//			cout << "found function "<< name <<endl;
			return m_vFunctions[i];
		}
	}
	return 0;
}
CFunction* CFunctionStorage::getFunction(int number)
{
	if(m_vFunctions.size()>number)
	{ 
		return m_vFunctions[number];
	}
	return 0;
}

int CFunctionStorage::getNumFunctions()
{
	return m_vFunctions.size();
}

void CFunctionStorage::deleteFunctions(){
	while(m_vFunctions.size()>0) {
		delete m_vFunctions.back();
		m_vFunctions.pop_back();
	}
}


