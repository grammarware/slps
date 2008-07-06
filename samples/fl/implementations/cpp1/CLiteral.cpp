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

#include "CLiteral.h"

CLiteral::CLiteral(int value) {
	v=value;
}
void CLiteral::accept(CVisitor* v){
	if(v) v->visit(this);
}
int CLiteral::getValue() {
	return v;
}
void CLiteral::setValue(int value) {
	v=value;
}
CExpr* CLiteral::clone() {
	return new CLiteral(v);
}
bool CLiteral::operator== (CExpr& other)
{
	CLiteral* l=dynamic_cast<CLiteral*>(&other);
	if(l==0)
		return false;
	return v==l->v;
}