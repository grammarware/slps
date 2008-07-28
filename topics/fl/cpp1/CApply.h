#ifndef CAPPLY_H
#define CAPPLY_H
#include "language.h"

using std::string;
using std::vector;
/**
	\brief This class represents function calls.
	
	It stores the name of the called function and the expressions, which are passed as arguments.
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CApply : public CExpr
{
	public:
		/**
		Constructor...
		@param name name of the function
		@param arguments a vector of CExpr-pointers, may be null if no arguments are needed
		*/
		CApply(string name, vector<CExpr*> * arguments=0);

		~CApply();
		virtual CExpr* clone();
		virtual void accept(CVisitor* v);
		CExpr *getArgument(unsigned int i);
		int getNumArguments();
		void addArgument(CExpr* e);
		string getFunction();
		virtual bool operator== (CExpr& other);
	private:
		string m_sFunction;
		vector<CExpr*> *m_eArguments;
};

#endif
