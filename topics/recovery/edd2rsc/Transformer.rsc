@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Transformer

import IO;
import String;
import ParseTree;

lexical EDDFile = {EDDLine [\n]}+;
lexical EDDLine = EDDName "=" EDDValue;
lexical EDDValue = ![\n]+;
syntax EDDName = // confix                
"start-grammar-symbol"		  
| "end-grammar-symbol"		  
| "start-comment-symbol"		  
| "end-comment-symbol"		  
| "start-label-symbol"		  
| "end-label-symbol"			  
| "start-nonterminal-symbol"	  
| "end-nonterminal-symbol"	  
| "start-terminal-symbol"		  
| "end-terminal-symbol"		  
| "start-special-symbol"		  
| "end-special-symbol"		  
| "start-group-symbol"		  
| "end-group-symbol"			  
| "start-optional-symbol"		  
| "end-optional-symbol"		  
| "start-repetition-star-symbol"			  
| "end-repetition-star-symbol"			  
| "start-repetition-plus-symbol"			  
| "end-repetition-plus-symbol"			  
| "start-seplist-star-symbol"	  
| "end-seplist-star-symbol"	  
| "start-seplist-plus-symbol"	  
| "end-seplist-plus-symbol"	  
// infix                          
| "terminator-symbol"			  
| "possible-terminator-symbol"  
| "defining-symbol"			  
| "multiple-defining-symbol"	  
| "definition-separator-symbol" 
| "concatenation-symbol"		  
| "inner-choice-symbol"		  
| "exception-symbol"			  
// postfix                        
| "postfix-option-symbol"	  
| "postfix-repetition-star-symbol"		  
| "postfix-repetition-plus-symbol"
// prefix                         
| "start-one-line-comment-symbol"
| // other                          
| "line-continuation-symbol"	  
| "tabulation-symbol"			  
| "empty-sequence-symbol"
| "nonterminals-may-contain"	
| "ignore"	  
;

// list[&T] slice(list[&T] lst, int start, int len)
list[&T] sliceFromTo(list[&T] lst, int s, int e) = slice(lst,s,e-s);

tuple[str name, str val] parseEDDLine((EDDLine)`<EDDName n>=<EDDValue v>`) = <"<n>","<v>">;

public map[str,str] loadEDD(loc f)
{
	map[str,str] bnf = ();
	for (line <- readFileLines(f))
	{
		try
		{
			tuple[str name, str val] t = parseEDDLine(parse(#EDDLine,line));
			if (t.name != "ignore")
				bnf[t.name] = replaceAll(replaceAll(t.val,"\\n","\n"),"\\t","\t");
		}
		catch _:
			println("Could not parse <line>, skipped over.");
		
	}
	return bnf;
}

public set[str] loadIgnored(loc f)
{
	list[str] l;
	l = for(line <- readFileLines(f))
		{
			tuple[str name, str val] t = parseEDDLine(parse(#EDDLine,line));
			if (t.name == "ignore")
				append t.val;
		}
	return {(x=="\\n")?"\n":x | x <- l};
	//return toSet(l);
}


map[str,str] quotedEscapes = ("\"" : "\\\"");
map[str,str] charClassEscapes = ("\"" : "\\\"","-" : "\\-", "\\" : "\\\\");

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	println("Loading the EDD from <src>...");
	map[str,str] edd = loadEDD(src);
	loc rsc = |cwd:///|+(args[1]+".rsc");
	println("Writing Rascal code to <rsc>...");
	writeFile(rsc,EDD2Rascal(edd,args[1]));
	// possible future work: ignored lines?
	println("Done.");
}

str EDD2Rascal(map[str,str] edd, str name)
{
	str prep = "module <name>
	'import util::IDE; // needed only for advanced IDE support (see last two lines)
	'start syntax <name>Grammar = <name>LayoutList <name>Production* <name>LayoutList;
	'syntax <name>Production = <name>Nonterminal <quoted(edd,"defining-symbol")> {<name>Definition <quoted(edd,"definition-separator-symbol")>}+ <quoted(edd,"terminator-symbol")>;
	'syntax <name>Definition = <name>Symbol+;
	'syntax <name>Symbol
 	' = @category=\"Identifier\" nonterminal: <name>Nonterminal
 	' | @category=\"Constant\" terminal: <name>Terminal";
 	if("start-group-symbol" in edd && edd["start-group-symbol"]!="" && edd["end-group-symbol"]!="")
		if ("definition-separator-symbol" in edd)
			prep += "\n | group: <quoted(edd,"start-group-symbol")> {<name>Definition <quoted(edd,"definition-separator-symbol")>}+ <quoted(edd,"end-group-symbol")>";
		else
 			prep += "\n | group: <quoted(edd,"start-group-symbol")> <name>Definition <quoted(edd,"end-group-symbol")>";
 	if("postfix-option-symbol" in edd && edd["postfix-option-symbol"]!="")
 		prep += "\n | optional: <name>Symbol <quoted(edd,"postfix-option-symbol")>";
 	if("postfix-repetition-star-symbol" in edd && edd["postfix-repetition-star-symbol"]!="")
 		prep += "\n | star: <name>Symbol <quoted(edd,"postfix-repetition-star-symbol")>";
 	if("postfix-option-symbol" in edd && edd["postfix-option-symbol"]!="")
 		prep += "\n | plus: <name>Symbol <quoted(edd,"postfix-repetition-plus-symbol")>";
 	if("start-seplist-star-symbol" in edd && edd["start-seplist-star-symbol"]!="" && edd["end-seplist-star-symbol"]!="")
 		prep += "\n | sepliststar: <quoted(edd,"start-seplist-star-symbol")> <name>Symbol <name>Symbol <quoted(edd,"end-seplist-star-symbol")>";
 	if("start-seplist-plus-symbol" in edd && edd["start-seplist-plus-symbol"]!="" && edd["end-seplist-plus-symbol"]!="")
 		prep += "\n | seplistplus: <quoted(edd,"start-seplist-plus-symbol")> <name>Symbol <name>Symbol <quoted(edd,"end-seplist-plus-symbol")>";
 	prep += ";"; // end of <name>Symbol
 	if("start-terminal-symbol" in edd && "end-terminal-symbol" in edd)
 		prep += "\nlexical <name>Terminal = <quoted(edd,"start-terminal-symbol")> <name>TerminalSymbol* <quoted(edd,"end-terminal-symbol")>;
 		'lexical <name>TerminalSymbol = ![<inlex(edd,"end-terminal-symbol")>];";
 	else
 		prep += "\nlexical <name>Terminal = ![ \\t-\\n]* !\>\> ![ \\t-\\n];";
 	if("start-nonterminal-symbol" in edd && "end-nonterminal-symbol" in edd)
 		prep += "\nlexical <name>Nonterminal = <quoted(edd,"start-nonterminal-symbol")> <name>NonterminalSymbol* <quoted(edd,"end-nonterminal-symbol")>;
 		'lexical <name>NonterminalSymbol = ![<inlex(edd,"end-nonterminal-symbol")>];";
 	else
 		prep += "\nlexical <name>Nonterminal = [A-Za-z_01-9<inlex(edd,"nonterminals-may-contain")>]+ !\>\> [A-Za-z_01-9<inlex(edd,"nonterminals-may-contain")>];";

	prep += "\nlayout <name>LayoutList = <name>Layout* !\>\> [\\t-\\n \\r \\ ]";
	bool comment = false;
	if ("start-one-line-comment-symbol" in edd)
	{
		prep += "!\>\> <quoted(edd,"start-one-line-comment-symbol")>";
		comment = true;
	}
	if ("start-multiline-comment-symbol" in edd && "end-multiline-comment-symbol" in edd)
	{
		prep += "!\>\> <quoted(edd,"start-multiline-comment-symbol")>";
		comment = true;
	}
	prep += ";\nlexical <name>Layout = [\\t-\\n \\r \\ ]";
	if (comment)
	{
		prep += " | <name>Comment ;\nlexical <name>Comment = @category=\"Comment\" ";
		if ("start-one-line-comment-symbol" in edd)
			prep += quoted(edd,"start-one-line-comment-symbol") + "![\\n]* [\\n]";
		if ("start-one-line-comment-symbol" in edd && "start-multiline-comment-symbol" in edd)
			prep += "\n| @category=\"Comment\" ";
		if ("start-multiline-comment-symbol" in edd && "end-multiline-comment-symbol" in edd)
			// @category="Comment" "/*" (![*] | [*] !>> [/])* "*/"
			prep += quoted(edd,"start-multiline-comment-symbol") + multilineCommentElement(edd["end-multiline-comment-symbol"]) + quoted(edd,"end-multiline-comment-symbol");
	}
	prep += ";\n"; 
	// the following is not necessary, but nice to have
	prep += "Tree get<name>(str s,loc z) = parse(#<name>Grammar,z);
	'public void register<name>() = registerLanguage(\"<name>\",\"<toLowerCase(name)>\",get<name>);";
	return prep;
}

str multilineCommentElement(str end)
{
	if (size(end)==1)
	{
		// one letter end metasymbol
		return "![" + escape(end,charClassEscapes) + "]*";
	}
	elseif (size(end)==2)
	{
		// two letter end metasymbol
		str first = "[" + escape(end[0],charClassEscapes) + "]";
		str second = "[" + escape(end[1],charClassEscapes) + "]";
		return "(!<first> | <first> !\>\> <second>)*";
	}
	elseif (size(end)==3)
	{
		// three letter end metasymbol
		str first = "[" + escape(end[0],charClassEscapes) + "]";
		str second = "[" + escape(end[1],charClassEscapes) + "]";
		str third = "[" + escape(end[1],charClassEscapes) + "]";
		return "(!<first> | <first> !\>\> <second> | <first> <second> !\>\> <third>)*";
	}
	else
		// give up
		return "NOT YET IMPLEMENTED";
}

map[str,str] makeCompleteEDDof(map[str,str] edd)
{
	map[str,str] r = ();
	for (key <- edd)
		if (key == "nonterminals-may-contain")
			r[key] = escape(edd[key],charClassEscapes);
		else
			r[key] = "\"" + escape(edd[key],charClassEscapes) + "\"";
	for (key <- hunter::Hunter::empty_edd)
		r[key] ?= "";
	return r;
}

str quoted(map[str,str] m, str k) = k in m? "\"" + escape(m[k],quotedEscapes) + "\"" : "";
str inlex(map[str,str] m, str k) = k in m? escape(m[k],charClassEscapes) : "";
