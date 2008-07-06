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
#ifndef PARSERERROR_H
#define PARSERERROR_H

#include <exception>
#include <string>
using namespace std;
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class ParserError : public exception
{
	public:
		string m_text;
		ParserError(string text);
		~ParserError() throw(){};

		virtual const char* what() const throw();
};

#endif
