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
#ifndef CEVALUATOR_H
#define CEVALUATOR_H

#include "CFunctionStorage.h"	
#include "CVisitor.h"
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CEvaluator : public CVisitor, public CFunctionStorage
{
	public:
		CEvaluator();

		virtual ~CEvaluator();

		virtual void visit ( CApply* e );
		virtual void visit ( CArgument* e );
		virtual void visit ( CBinary* e );
		virtual void visit ( CExpr* e );
		virtual void visit ( CIfThenElse* e );
		virtual void visit ( CLiteral* e );
		virtual void visit ( CFunction* e );
//	private:
		int result;
	private:
		int getArgumentValue(const string& name);
		vector <map<string,int>*> m_vContext;
		void pushContext(map<string,int> *context);
		map<string,int>* popContext();
		map<string,int>* getContext();
};

#endif
