//
// C++ Interface: ParserError
//
// Description:
//
//
// Author: Andreas Mützel <amuetzel@uni-koblenz.de>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef EVALUATIONERROR_H
#define EVALUATIONERROR_H

#include <exception>
#include <string>
using namespace std;
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class EvaluationError : public exception
{
	public:
		string m_text;
		EvaluationError(string text);
		~EvaluationError() throw(){};

		virtual const char* what() const throw();
};

#endif
