#include "EvaluationError.h"

EvaluationError::EvaluationError(string text)
		: exception(), m_text(text)
{
}

const char* EvaluationError::what() const throw()
{
	return string("Evaluation Error: "+m_text).c_str();
}



