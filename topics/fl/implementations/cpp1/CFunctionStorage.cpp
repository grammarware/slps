#include "CFunctionStorage.h"

CFunctionStorage::CFunctionStorage()
{
}


CFunctionStorage::~CFunctionStorage()
{
	deleteFunctions();
}

void CFunctionStorage::addFunction(/*const string& name, */CFunction* function) 
{
	for(int i=0; i<m_vFunctions.size();i++) {
		if(m_vFunctions[i]->getName()==function->getName()) {
//			cout << "found function "<< name <<endl;
			cerr << "Warnung: function "<<function->getName()<<" already exists, ignoring new definition"<<endl;  
			return;
		}
	}
//	cout << "adding function "<< function->getName()<<endl;
	m_vFunctions.push_back(function);
}

CFunction* CFunctionStorage::getFunction(const string& name)
{
	for(int i=0; i<m_vFunctions.size();i++) {
		if(m_vFunctions[i]->getName()==name) {
//			cout << "found function "<< name <<endl;
			return m_vFunctions[i];
		}
	}
	return 0;
}
CFunction* CFunctionStorage::getFunction(int number)
{
	if(m_vFunctions.size()>number)
	{ 
		return m_vFunctions[number];
	}
	return 0;
}

int CFunctionStorage::getNumFunctions()
{
	return m_vFunctions.size();
}

void CFunctionStorage::deleteFunctions(){
	while(m_vFunctions.size()>0) {
		delete m_vFunctions.back();
		m_vFunctions.pop_back();
	}
}


