#ifndef _CLITERAL_HEADER_INCLUDED_
#define _CLITERAL_HEADER_INCLUDED_
#include "language.h"


/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief The class for literals
 */

class CLiteral: public CExpr {
	public:
		CLiteral(int value);
		virtual void accept(CVisitor* v);
		virtual ~CLiteral(){};
		virtual CExpr* clone();
		int getValue();
		void setValue(int i);
		/*int operator() (void) {
			return v;
		};
		friend ostream& operator <<(ostream &os,const CLiteral &obj){
			os << obj.v;
			return os;
		};*/
		virtual bool operator== (CExpr& other);
	protected: 
		int v;
};
#endif
