@contributor{Jurgen Vinju - Jurgen.Vinju@cwi.nl - SWAT, CWI}
@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Busses

import IO;
import Set;
import String;
import ParseTree;

start syntax System = Line+;
syntax Line = Num ":" {Id ","}+ "." ;
layout WS = [\ \t\n\r]* !>> [\ \t\n\r];
lexical Id = [A-Za-z][A-Za-zŸŠš§\-\ ]+[A-Za-z] !>> [A-Za-z];
lexical Num = [0-9]+ !>> [0-9];

rel[Id,Id] extractGraph(loc source) = 
{<from,to> | /Line b := parse(#start[System],source), (Line)`<Num _>: <{Id ","}* _>, <Id from>, <Id to>, <{Id ","}* _>.` := b};	

bool kannUmsteigen(rel[Id,Id] sys, Id hs) = size(sys[hs]) > 1;

void synthesizeDotGraph(loc source, loc target)
{
	rel[Id from,Id to] conn = extractGraph(source);
	writeFile(target,
		"digraph Metro { node [shape=box]
		'<for (<from, to> <- conn) {>
		' \"<from>\" -\> \"<to>\"<}>
		'<for (st <- conn<from>, kannUmsteigen(conn, st)){>
		' \"<st>\" [shape=ellipse]<}>
		'}");
}

public void main()
{
	synthesizeDotGraph(	|home:///projects/slps/topics/implementation/busses/Koblenz.bus|,
						|home:///projects/slps/topics/implementation/busses/Koblenz.dot|);
}

