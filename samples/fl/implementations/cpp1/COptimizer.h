#ifndef COPTIMIZER_H
#define COPTIMIZER_H

#include "CVisitor.h"
#include "language.h"

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Visitor that optimizes any expression.
	
	This visitor is used to optimize expressions. It creates an optimized copy of the object it visits, so the original expression remains unchanged.
	Can not yet be used to optimize functions, another TODO...
 */
class COptimizer : public CVisitor
{
	public:
		COptimizer();

		~COptimizer();

		virtual void visit ( CApply* e );
		virtual void visit ( CArgument* e );
		virtual void visit ( CBinary* e );
		virtual void visit ( CExpr* e );
		virtual void visit ( CFunction* e );
		virtual void visit ( CIfThenElse* e );
		virtual void visit ( CLiteral* e );
		
		/**
		Returns the resulting Expression.
		*/
		CExpr* getResult();		
	private:
		CExpr* result;
};

#endif
