//
// C++ Implementation: ParserError
//
// Description:
//
//
// Author: Andreas Mützel <amuetzel@uni-koblenz.de>, (C) 2008
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "EvaluationError.h"

EvaluationError::EvaluationError(string text)
		: exception(), m_text(text)
{
}

const char* EvaluationError::what() const throw()
{
	return string("Evaluation Error: "+m_text).c_str();
}



