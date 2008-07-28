#ifndef CFUNCTIONSTORAGE_H
#define CFUNCTIONSTORAGE_H
#include "CFunction.h"
#include <vector>
using namespace std;
/**
	@author Andreas Mützel <amuetzel@uni-koblenz.de>
*/
class CFunctionStorage
{
	public:
		CFunctionStorage();

		virtual ~CFunctionStorage();
		void addFunction(/*const string& name,*/ CFunction* function) ;
		CFunction* getFunction(const string& name);
		CFunction* getFunction(int number);
		int getNumFunctions();
		void deleteFunctions();
	private:
		vector <CFunction*> m_vFunctions;

};

#endif
