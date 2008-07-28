#ifndef _CEXPR_HEADER_INCLUDED_
#define _CEXPR_HEADER_INCLUDED_
#include "language.h"
#include <iostream>
using namespace std;
class CFunctionStorage;

/**
	@brief Base class for all expressions.
	*/

class CExpr {
	public: 
		/**
			accepts a visitor.
			@param v a CVisitor*
		*/
		virtual void accept(CVisitor* v)=0;
		virtual ~CExpr() {};
		/**
			Returns a pointer to a deep clone of the expression.	
		*/
		virtual CExpr* clone()=0;
		/**
			Compares two expressions.
		*/
		virtual bool operator== (CExpr& other) {
			return false;
		}
		/**
			Prints an expression by using a temporary CPrettyPrinter-Visitor.
		*/
		friend ostream& operator<<(ostream &os, CExpr &obj);
	private:
};
#endif