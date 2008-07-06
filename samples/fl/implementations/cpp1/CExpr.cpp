#include "language.h"
#include "CPrettyPrinter.h"
ostream& operator<<(ostream &os, CExpr &obj){
//			os << obj.print();
	CPrettyPrinter p(os);
	obj.accept(&p);
	return os;
};