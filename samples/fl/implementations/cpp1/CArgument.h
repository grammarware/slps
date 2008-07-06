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
#ifndef CARGUMENT_H
#define CARGUMENT_H
#include "language.h"

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief Represents named parameters.
 */
class CArgument : public CExpr
{
	public:
		/**
		Constructor...
		@param name The name of the Argument. 
		*/
		CArgument ( string name );

		~CArgument();

		virtual CExpr* clone();
		virtual void accept ( CVisitor* v );
		virtual string getName(){return m_sName;};
		virtual bool operator== (CExpr& other);
	private:
		string m_sName;
};

#endif
