#ifndef EVALUATIONERROR_H
#define EVALUATIONERROR_H

#include <exception>
#include <string>
using namespace std;
/**
	@brief This class represents all Errors that may occur during evaluation.
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
