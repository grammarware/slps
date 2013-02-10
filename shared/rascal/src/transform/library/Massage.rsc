@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{massage}
module transform::library::Massage

import language::BGF;
import language::XScope;
import language::XOutcome;
import normal::BGF;
import diff::GDT;
import transform::library::Brutal;
import List;

bool massage_eq({selectable(_,x),x}) = true; // deprecated, please use anonymize/deanonymize instead!
bool massage_eq({optional(selectable(s,x)),selectable(s,optional(x))}) = true;
bool massage_eq({star(selectable(s,x)),selectable(s,star(x))}) = true;
bool massage_eq({plus(selectable(s,x)),selectable(s,plus(x))}) = true;

bool massage_eq({optional(optional(x)),optional(x)}) = true;
bool massage_eq({optional(star(x)),star(x)}) = true;
bool massage_eq({optional(plus(x)),star(x)}) = true;
bool massage_eq({star(optional(x)),star(x)}) = true;
bool massage_eq({star(star(x)),star(x)}) = true;
bool massage_eq({star(plus(x)),star(x)}) = true;
bool massage_eq({plus(optional(x)),star(x)}) = true;
bool massage_eq({plus(star(x)),star(x)}) = true;
bool massage_eq({plus(plus(x)),plus(x)}) = true;

bool massage_eq({sequence([star(x),star(x)]),star(x)}) = true;
bool massage_eq({sequence([optional(x),star(x)]),star(x)}) = true;
bool massage_eq({sequence([star(x),optional(x)]),star(x)}) = true;
bool massage_eq({sequence([optional(x),plus(x)]),plus(x)}) = true;
bool massage_eq({sequence([plus(x),optional(x)]),plus(x)}) = true;
bool massage_eq({sequence([plus(x),star(x)]),plus(x)}) = true;
bool massage_eq({sequence([star(x),plus(x)]),plus(x)}) = true;
bool massage_eq({sequence([x,star(x)]),plus(x)}) = true;
bool massage_eq({sequence([star(x),x]),plus(x)}) = true;
// separator lists
bool massage_eq({sequence([x,optional(sequence([y,x]))]),sequence([optional(sequence([x,y])),x])}) = true;
bool massage_eq({sequence([x,plus(sequence([y,x]))]),sequence([plus(sequence([x,y])),x])}) = true;
bool massage_eq({sequence([x,star(sequence([y,x]))]),sequence([star(sequence([x,y])),x])}) = true;
bool massage_eq({sequence([x,star(sequence([y,x]))]),seplistplus(x,y)}) = true;
bool massage_eq({sequence([star(sequence([x,y])),x]),seplistplus(x,y)}) = true;
bool massage_eq({optional(sequence([x,star(sequence([y,x]))])),sepliststar(x,y)}) = true;
bool massage_eq({optional(sequence([star(sequence([x,y])),x])),sepliststar(x,y)}) = true;
bool massage_eq({optional(seplistplus(x,y)),sepliststar(x,y)}) = true;

// Boolean grammars
bool massage_eq({not(not(x)),x}) = true;
bool massage_eq({not(selectable(s,x)),selectable(s,not(x))}) = true;
bool massage_eq({not(allof([x,y])),choice([not(x),not(y)])}) = true;
bool massage_eq({not(allof([x,y])),choice([not(y),not(x)])}) = true;
bool massage_eq({not(allof([not(x),not(y)])),choice([x,y])}) = true;
bool massage_eq({not(allof([not(x),not(y)])),choice([y,x])}) = true;
bool massage_eq({not(choice([x,y])),allof([not(x),not(y)])}) = true;
bool massage_eq({not(choice([x,y])),allof([not(y),not(x)])}) = true;
bool massage_eq({not(choice([not(x),not(y)])),allof([x,y])}) = true;
bool massage_eq({not(choice([not(x),not(y)])),allof([y,x])}) = true;

default bool massage_eq(set[BGFExpression] s)
{
	// some of the following are not general enough
	
	if ({choice(L),z} := s)
		{
			if (optional(x) := z)
			{
				if ((x in L || optional(x) in L) && epsilon() in L) return true;
				if ({optional(x),x} := toSet(L)) return true;
			}
			if (star(x) := z)
			{
				if ((star(x) in L || plus(x) in L) && epsilon() in L) return true;
				if ({star(x),x} := toSet(L)) return true;
				if ({star(x),optional(x)} := toSet(L)) return true;
				if ({star(x),plus(x)} := toSet(L)) return true;
				if ({plus(x),optional(x)} := toSet(L)) return true;
			}
			if (plus(x) := z, {plus(x),x} := toSet(L)) return true;
			
			L1 = visit(L){case selectable(_,BGFExpression e) => e};
			if (eqE(normalise(choice(L1)),z)) return true;
		}
	return false;
}

XBGFResult runMassage(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	if (massage_eq({e1,e2}))
		return transform::library::Brutal::runReplace(e1,e2,w,g);
	else
		return <problemExpr2("Expressions are not massage-equivalent.",e1,e2),g>;
}
