#ifndef CEVALUATOR_H
#define CEVALUATOR_H

#include "CFunctionStorage.h"	
#include "CVisitor.h"
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CEvaluator : public CVisitor, public CFunctionStorage
{
	public:
		CEvaluator();

		virtual ~CEvaluator();

		virtual void visit ( CApply* e );
		virtual void visit ( CArgument* e );
		virtual void visit ( CBinary* e );
		virtual void visit ( CExpr* e );
		virtual void visit ( CIfThenElse* e );
		virtual void visit ( CLiteral* e );
		virtual void visit ( CFunction* e );
//	private:
		int result;
	private:
		int getArgumentValue(const string& name);
		vector <map<string,int>*> m_vContext;
		void pushContext(map<string,int> *context);
		map<string,int>* popContext();
		map<string,int>* getContext();
};

#endif
