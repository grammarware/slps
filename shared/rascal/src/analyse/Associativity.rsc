@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Associativity

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import io::ReadBGF;
import lib::Rascalware;
import analyse::Metrics;
import normal::BGF;
import transform::XBGF;
import transform::CBGF;
import export::BNF;
import Relation;
import IO;
import analyse::Layers; // for testing only

public CBGFSequence removeAssociativity(rel[str,str] ass, BGFGrammar g)
{
	//= [unite_splitN(l<0>, prodsOfN( l<1>, g.prods), globally()) | l <- layers];
	CBGFSequence c = [];
	for (<n,x> <- ass)
	{
		ps = prodsOfN(n,g.prods);
		//
		if (production(str l,n,rhs:sequence([nonterminal(n),star(sequence([nonterminal(x),nonterminal(n)]))])) <- ps)
		{
			// simplest case: we have the production right there
			if (l=="")
				c += designate_unlabel(production("tmplabel",n,rhs));
			newrhs = sequence([nonterminal(n),nonterminal(x),nonterminal(n)]);
			c += assoc_iterate(production(l==""?"tmplabel":l,n,newrhs));
			if (l=="")
				c += unlabel_designate(production("tmplabel",n,newrhs));
		}
		else
			println("<pp(ps)>");
		// horizontal production rules
		for (production(str l,n,choice(L)) <- ps,
			e <- L,
			nonterminal(y) := e,
			production(str l2,y,rhs:sequence([nonterminal(n),star(sequence([nonterminal(x),nonterminal(n)]))])) <- prodsOfN(y,g.prods))
		{
			// the production is one chain step ahead
			if (l2=="")
				c += designate_unlabel(production("tmplabel",y,rhs));
			newrhs = sequence([nonterminal(n),nonterminal(x),nonterminal(n)]);
			c += assoc_iterate(production(l2==""?"tmplabel":l2,y,newrhs));
			if (l2=="")
				c += unlabel_designate(production("tmplabel",y,newrhs));
		}
		// horizontal production rules with selectables
		for (production(str l,n,choice(L)) <- ps,
			e <- L,
			selectable(_,nonterminal(y))  := e,
			production(str l2,y,rhs:sequence([selectable(str name_n1,nonterminal(n)),star(sequence([selectable(str name_x,nonterminal(x)),selectable(str name_n2,nonterminal(n))]))])) <- prodsOfN(y,g.prods))
		{
			// the production is one chain step ahead
			if (l2=="")
				c += designate_unlabel(production("tmplabel",y,rhs));
			l3 = l2==""?"tmplabel":l2;
			c +=
				[
					anonymize_deanonymize(production(l3,y,sequence([marked(selectable(name_n1,nonterminal(n))),star(sequence([marked(selectable(name_x,nonterminal(x))),marked(selectable(name_n2,nonterminal(n)))]))]))),
					assoc_iterate(production(l3,y,sequence([nonterminal(n),nonterminal(x),nonterminal(n)]))),
					deanonymize_anonymize(production(l3,y,sequence([marked(selectable(name_n1,nonterminal(n))),marked(selectable(name_x,nonterminal(x))),marked(selectable(name_n2,nonterminal(n)))])))
				];
			if (l2=="")
				c += unlabel_designate(production("tmplabel",y,sequence([selectable(name_n1,nonterminal(n)),selectable(name_x,nonterminal(x)),selectable(name_n2,nonterminal(n))])));
		}
	}
	return c;
}

BGFGrammar mynorm(BGFGrammar g)
{
	g = visit(g)
		{
			case selectable(s,e) => e
		}
	// TODO: replace with external mutation verticalise-all
	for (p <- g.prods, production(_,str n,choice(L)) := p)
		g = transform([vertical(innt(n))],g);
	for (n <- [n | n <- usedNs(g), len(prodsOfN(n,g.prods))==1, len([n | /nonterminal(n) := g])==1])
		g = transform([inline(n)],g);
	return g;
}

public rel[str,str] detectAssociativity(BGFGrammar g)
 = {<n,x> |	p <- mynorm(g).prods,
 	production(_,str n,sequence([nonterminal(n),star(sequence([nonterminal(str x),nonterminal(n)]))])) := p ||
 	production(_,str n,sequence([star(sequence([nonterminal(n),nonterminal(x)])),nonterminal(n)])) := p };

public void main()
{
	for (src <- ["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"])
	{
		println("Reading <src>...");
		BGFGrammar g = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
		println("Analysing <src>...");
		g = transform(forward(removeLayers(detectLayers(g),g)),g);
		rel[str,str] ass = detectAssociativity(g);
		if(!isEmpty(ass))
		{
			println("Found <ass>");
			iprintln(removeAssociativity(ass,g));
			println(pp(transform(forward(removeAssociativity(ass,g)),g)));
		}
	}
}