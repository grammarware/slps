@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::GraphML2MegaL

import structure::MegaADT;
import lang::xml::DOM;
//import IO;
import String;

map[str,str] nodes = ();

AMegaModel readGraphML(loc f)
{
	if (document(element(namespace(_,"http://graphml.graphdrawing.org/xmlns"),"graphml",L)) := parseXMLDOMTrim(readFile(f)))
		return megamodel(replaceLast(f.file,"."+f.extension,""), "", [],
			[mapnode(n) | /element(_,"node",n) := L],
			[mapedge(m)  | /element(_,"edge",m) := L]);
	else
		throw "<f> is not a proper GraphML file";
}

MegaDeclaration mapnode(list[Node] top)
{
	str name = "";
	str id = "";
	MegaMod m = nomod();
	if (/attribute(none(),"id",str nodeid) := top)
		id = nodeid;
	if (/element(namespace(_,"http://www.yworks.com/xml/graphml"),"GenericNode",n) := top)
	{
		if (/element(namespace(_,"http://www.yworks.com/xml/graphml"),"NodeLabel",label) := n)
		{
			if ([A*,charData(truename),B*] := label)
				name = truename;
			if ([A*,attribute(none(),"fontStyle","bolditalic"),B*] := label)
				m = variable();
			if ([A*,attribute(none(),"fontStyle","italic"),B*] := label)
				m = variable();
		}
		nodes[id] = name;
		if (/element(namespace(_,"http://www.yworks.com/xml/graphml"),"Fill",[A*,attribute(none(),"color",col),B*]) := n)
		{
			//println("The colour of <name> is <col>.");
			switch(col)
			{
				case "#3366FF": return artifact(m, name, false, "");
				case "#FFCC00": return language(m, name, false, "");
				case "#339966": return functionapp(m, name, false, "");
				case "#99FF99": return function(m, name, false, "");
				///////////////
				default: return artifact(m, name, false, "");
			}
		}
	}
}

MegaRelation mapedge(list[Node] top)
{
	str src = "", tgt = "";
	if (/attribute(_,"source",str source) := top)
		src = nodes[source];
	if (/attribute(_,"target",str target) := top)
		tgt = nodes[target];
	if (/element(namespace(_,"http://www.yworks.com/xml/graphml"),"EdgeLabel",[A*,charData(r),B*]) := top)
		switch(r)
		{
			case "subsetOf": return subsetOf(src,tgt,"");
			case "elementOf": return elementOf(src,tgt,"");
			case "partOf": return partOf(src,tgt,"");
			case "correspondsTo": return correspondsTo(src,tgt,"");
			case "dependsOn": return dependsOn(src,tgt,"");
			case "refersTo": return refersTo(src,tgt,"");
			case "conformsTo": return conformsTo(src,tgt,"");
			case "realizationOf": return realizationOf(src,tgt,"");
			case "descriptionOf": return descriptionOf(src,tgt,"");
			case "definitionOf": return definitionOf(src,tgt,"");
			case "inputOf": return inputOf(src,tgt,"");
			case "hasOutput": return hasOutput(src,tgt,"");
			case "domainOf": return domainOf(src,tgt,"");
			case "hasRange": return hasRange(src,tgt,"");
			/////////////////////////////////////////////
			default: return partOf(src,tgt,"");
		}
}
