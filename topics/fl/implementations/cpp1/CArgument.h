#ifndef CARGUMENT_H
#define CARGUMENT_H
#include "language.h"

/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
	@brief Represents named parameters.
 */
class CArgument : public CExpr
{
	public:
		/**
		Constructor...
		@param name The name of the Argument. 
		*/
		CArgument ( string name );

		~CArgument();

		virtual CExpr* clone();
		virtual void accept ( CVisitor* v );
		virtual string getName(){return m_sName;};
		virtual bool operator== (CExpr& other);
	private:
		string m_sName;
};

#endif
