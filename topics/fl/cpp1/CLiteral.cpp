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