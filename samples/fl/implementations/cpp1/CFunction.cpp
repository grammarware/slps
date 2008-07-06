#include "CFunction.h"
#include "CPrettyPrinter.h"

CFunction::CFunction(string name, vector<CArgument*> *args, CExpr * expr)
{
//	cout <<"Function "<<name<<" created\n";
	m_sName=name;
	m_vArguments=args;
	m_eExpression=expr;
}


CFunction::~CFunction()
{
	CExpr *tmp;
//	cout << "CFunction destructor\n";
	if(m_vArguments) {
		while(m_vArguments->size() > 0) {
			tmp=m_vArguments->back();
			if(tmp)
				delete tmp;
			m_vArguments->pop_back();
		}
		delete m_vArguments;
	}
	delete m_eExpression;
}
string CFunction::getName() 
{
//	cout<< "function "<< m_sName << " queried"<<endl;
	return m_sName;
}
CExpr* CFunction::getExpression() 
{
	return m_eExpression;
}
int CFunction::getNumArguments()
{
	if(!m_vArguments) 
		return 0;
	return m_vArguments->size();
}
CArgument* CFunction::getArgument(unsigned int i) {
	if(!m_vArguments) 
		return 0;
	
	if(i>=m_vArguments->size()) {
		return 0; //ERROR
	}
	return m_vArguments->at(i);
}

void CFunction::accept(CVisitor* v)
{
	v->visit(this);
}

ostream& operator<<(ostream &os, CFunction &obj)
{
	CPrettyPrinter p(os);
	obj.accept(&p);
	return os;
}