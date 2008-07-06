#ifndef CFUNCTION_H
#define CFUNCTION_H
#include "language.h"

/**
	@brief 
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CFunction
{
	public:
		CFunction(string name, vector<CArgument*> *args, CExpr * expr);
		virtual ~CFunction();
		string getName();
		CExpr* getExpression();
		int getNumArguments();
		CArgument* getArgument(unsigned int i);
		void accept(CVisitor* v);
		friend ostream& operator<<(ostream &os, CFunction &obj);
	protected:
		string m_sName;
		vector<CArgument*> *m_vArguments;
		CExpr *m_eExpression;
};

#endif
