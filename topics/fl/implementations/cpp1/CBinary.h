#ifndef CBinary_h
#define CBinary_h

#include "language.h"

enum BinaryOpType {
	EQUALS,
 PLUS,
 MINUS
};

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>	
	@brief Represents binary Expressions.
	
	This Class represents 3 types of binary expressions: Plus, Minus and Equals	
 */
class CBinary : public CExpr
{
	public:
		/**
		Constructor...
		@param left the first sub-expression
		@param right the second sub-expression
		@param type BinaryOpType, describes which operation to perform 
		 */
		CBinary(CExpr* left, CExpr* right, BinaryOpType type);

		~CBinary();

		virtual void accept(CVisitor* v);
    
		virtual CExpr *clone();
	
		CExpr *getLeft(), *getRight();
		BinaryOpType getType();
		void setLeft(CExpr* e), setRight(CExpr* e);
		void setType(BinaryOpType type);
		virtual bool operator== (CExpr& other);
	
	private:
		CExpr* l;
		CExpr* r;	
		BinaryOpType t;
};

#endif
