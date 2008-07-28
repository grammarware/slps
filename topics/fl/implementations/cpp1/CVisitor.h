#ifndef _CVISITOR_HEADER_INCLUDED_
#define _CVISITOR_HEADER_INCLUDED_

class CExpr;
class CApply;
class CArgument;
class CBinary;
class CIfThenElse;
class CLiteral;
class CFunction;

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Base class for all visitors.
	
	Base class for visitors that can visit all CExpr-objects and also objects of class CFunction.
 */
class CVisitor {
	public: 
		virtual void visit(CExpr* e)=0;
		virtual void visit(CApply* e)=0;
		virtual void visit(CArgument* e)=0;
		virtual void visit(CBinary* e)=0;
		virtual void visit(CIfThenElse* e)=0;
		virtual void visit(CLiteral* e)=0;
		virtual void visit(CFunction* e)=0;
};


#endif