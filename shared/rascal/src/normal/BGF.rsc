@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module normal::BGF

import syntax::BGF;
// needed for levels 2+
import lib::Rascalware;
import analyse::Metrics;
import syntax::XBGF;
import transform::XBGF;

public BGFGrammar normalise(BGFGrammar g) = grammar (g.roots, normalise(g.prods));

// remove duplicate production rules
public list[BGFProduction] normalise([L1*,BGFProduction X1,L2*,X1,L3*])
						= normalise([*L1,X1,*L2,*L3]);

public list[BGFProduction] normalise(list[BGFProduction] prods)
						= [normalise(p) | p <- prods]; 

// the following is kinda useful, but breaks some code if normalisations are not done after each step
public BGFProduction normalise(production ("", str lhs, selectable(str label,BGFExpression rhs)))
							 = production (label, lhs, normalise(rhs));

public default BGFProduction normalise(BGFProduction p)
							 = production (p.label, p.lhs, normalise(p.rhs));

public BGFExpression normalise(BGFExpression e)
{
	// Algebraic normalisations; TODO: others needed?
	return innermost visit(e)
	{
		case sequence([]) => epsilon()
		case sequence([BGFExpression e]) => e
		case choice([]) => empty()
		case choice([BGFExpression e]) => e
		case optional(epsilon()) => epsilon()
		case plus(epsilon()) => epsilon()
		case star(epsilon()) => epsilon()
		case sepliststar(X,epsilon()) => star(X)
		case seplistplus(X,epsilon()) => plus(X)
		case sepliststar(epsilon(),X) => star(X)
		case seplistplus(epsilon(),X) => star(X)
		case sequence([L1*,sequence(L),L2*]) => sequence(L1+L+L2)
		case sequence([L1*,epsilon(),L2*]) => sequence(L1+L2)
		case sequence([L1*,empty(),L2*]) => empty()
		case choice([L1*,choice(L),L2*]) => choice(L1+L+L2)
		case choice([L1*,empty(),L2*]) => choice(L1+L2)
		case choice([L1*,X1,L2*,X1,L3*]) => choice([*L1,X1,*L2,*L3])
	};
}

public BGFGrammar tw(BGFExpression e)
{
	return grammar([],[production("","foo",e)]);
}

test bool n1() {return normalise(tw(sequence([])))==tw(epsilon());}
test bool n2() {return normalise(tw(sequence([terminal("1")])))==tw(terminal("1"));}
test bool n3() {return normalise(tw(choice([])))==tw(empty());}
test bool n4() {return normalise(tw(choice([terminal("1")])))==tw(terminal("1"));}
test bool n5() {return normalise(tw(optional(epsilon())))==tw(epsilon());}
test bool n6() {return normalise(tw(plus(epsilon())))==tw(epsilon());}
test bool n7() {return normalise(tw(star(epsilon())))==tw(epsilon());}
test bool n8() {return normalise(tw(sequence([terminal("1"),sequence([terminal("2"),terminal("3")])])))
                               ==tw(sequence([terminal("1"),terminal("2"),terminal("3")]));}
test bool n9() {return normalise(tw(sequence([sequence([terminal("1"),terminal("2")]),terminal("3")])))
                               ==tw(sequence([terminal("1"),terminal("2"),terminal("3")]));}
test bool n10(){return normalise(tw(sequence([terminal("1"),epsilon(),terminal("2"),epsilon()])))
                               ==tw(sequence([terminal("1"),terminal("2")]));}
test bool n11(){return normalise(tw(choice([terminal("1"),choice([terminal("2"),terminal("3")])])))
                               ==tw(choice([terminal("1"),terminal("2"),terminal("3")]));}
test bool n12(){return normalise(tw(choice([choice([terminal("1"),terminal("2")]),terminal("3")])))
                               ==tw(choice([terminal("1"),terminal("2"),terminal("3")]));}
test bool n13(){return normalise(tw(choice([terminal("1"),terminal("2"),terminal("2"),terminal("3")])))
                               ==tw(choice([terminal("1"),terminal("2"),terminal("3")]));}
test bool n14(){return normalise(production("","N",sequence([epsilon(),nonterminal("a"),nonterminal("b")])))
							  == production("","N",sequence([nonterminal("a"),nonterminal("b")]));}

// everything below is highly experimental, use at your own risk! --VVZ

// no named subexpressions
public BGFGrammar normalise2(BGFGrammar g) = visit(g) { case selectable(s,e) => e } ;

// no named subexpressions, no chain production rules
public BGFGrammar normalise3(BGFGrammar g)
{
	g = normalise2(g);
	for (n <- [n | n <- usedNs(g), len(prodsOfN(n,g.prods))==1, len([n | /nonterminal(n) := g])==1])
		g = transform([inline(n)],g);
	return g;
}

// no named subexpressions, no chain production rules, no terminals
public BGFGrammar normalise4(BGFGrammar g)
{
	g = visit(g)
		{
			case selectable(s,e) => e
			case terminal(_) => epsilon()
		}
	for (n <- [n | n <- usedNs(g), len(prodsOfN(n,g.prods))==1, len([n | /nonterminal(n) := g])==1])
		g = transform([inline(n)],g);
	return g;
}
