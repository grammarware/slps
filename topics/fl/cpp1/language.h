#ifndef LANGUAGE_INCLUDED
#define LANGUAGE_INCLUDED
#include <iostream>
#include <string>
#include <vector>
#include <map>
using namespace std;


class CExpr;
class CArgument;
class CBinary;
class CFunction;

class CVisitor;
class CApply;
class CEvaluator;
class CLiteral;
class CIfThenElse;
#include "CExpr.h"
#include "CArgument.h"
#include "CBinary.h"
#include "CFunction.h"

#include "CApply.h"
#include "CIfThenElse.h"
#include "CLiteral.h"
#include "CVisitor.h"		
#endif