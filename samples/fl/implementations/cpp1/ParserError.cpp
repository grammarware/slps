#include "ParserError.h"

ParserError::ParserError(string text)
		: exception(), m_text(text)
{
}

const char* ParserError::what() const throw()
{
	return string("Parser Error: "+m_text).c_str();
}



