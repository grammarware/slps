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
#include "ParserError.h"

ParserError::ParserError(string text)
		: exception(), m_text(text)
{
}

const char* ParserError::what() const throw()
{
	return string("Parser Error: "+m_text).c_str();
}



