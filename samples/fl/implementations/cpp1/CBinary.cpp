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
#include "CBinary.h"

CBinary::CBinary(CExpr* left, CExpr* right, BinaryOpType type)
 : CExpr()
{
	l=left;
	r=right;
	t=type;
}


CBinary::~CBinary()
{
	if(l) delete l;
	if(r) delete r;
}


void CBinary::accept(CVisitor* v)
{
	if(v) v->visit(this);
}

CExpr* CBinary::clone() 
{
	return new CBinary(l->clone(),r->clone(),t);
}

CExpr* CBinary::getLeft() {
	return l;
}
CExpr* CBinary::getRight() {
	return r;
}
BinaryOpType CBinary::getType() {
	return t;
}
void CBinary::setLeft(CExpr* e) {
	l=e;
}
void CBinary::setRight(CExpr* e) {
	r=e;
}
void CBinary::setType(BinaryOpType type) {
	t=type;
}

bool CBinary::operator== (CExpr& other)
{
	CBinary* b= dynamic_cast<CBinary*>(&other);
	if(b==0)
		return false;
	
	return (l==b->l)&&(r==b->r)&&(t==b->t);
}