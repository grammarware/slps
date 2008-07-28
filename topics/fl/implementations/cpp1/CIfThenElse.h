#ifndef CIFTHENELSE_H
#define CIFTHENELSE_H

#include "language.h"

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief "The If-Then-Else"-expression
	
*/
class CIfThenElse : public CExpr
{
	public:
		/**
		Constructor...
		@param condition The expression to be used as the condition, if it evaluates to 0 then the else-part will be evaluated, else the if-part.
		@param ifPart The IF-part
		@param elsePart The ELSE-part
		*/
		CIfThenElse(CExpr* condition, CExpr *ifPart, CExpr *elsePart);

		~CIfThenElse();

		virtual CExpr* clone();
		virtual void accept ( CVisitor* v );
		CExpr *getCondition();
		CExpr *getIfPart();
		CExpr *getElsePart();
		virtual bool operator== (CExpr& other);
	protected:
		CExpr *m_eCondition, *m_eIfPart, *m_eElsePart;
};

#endif
