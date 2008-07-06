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