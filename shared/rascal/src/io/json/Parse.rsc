@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::json::Parse

import io::json::Syntax;
import IO;
import String;
import ParseTree;
import util::Math;

data JSO
	= jsnumber(real n)
	| jsstring(str s)
	| jsboolean(bool b)
	| jsarray(list[JSO] xs)
	| jsobject(map[JSO,JSO] kvs)
	| jsnull()
	;

JSO str2jso(str s)
{
	PT = parse(#BasicType,trim(s));
	return basic2jso(PT);
}
	
JSO basic2jso(BasicType t)
{
	//if ((BasicType)`Number v` := PT)
	//println("numberr!");
	switch(t)
	{
		case (BasicType)`<Number v>`: return str2num("<v>");
		case (BasicType)`<String v>`: return str2str("<v>");
		case (BasicType)`<Boolean v>`: return str2bool("<v>");
		case (BasicType)`<Array v>`: return arr2array(v);
		case (BasicType)`<Object v>`: return obj2object(v);
		case (BasicType)`<Null v>`: return jsnull();
		default:
			println("dunno");
	}
	return jsnull();
}

JSO str2num(str s) = jsnumber(toReal(s)); // TODO so that it recognizes ints? (not crucial)
JSO str2str(str s) = jsstring(s);
JSO str2bool("true") = jsboolean(true);
default JSO str2bool(str s) = jsboolean(false);

list[JSO] arr2array((Array)`[<{BasicType ","}* vs>]`) = [basic2jso(v) | BasicType v <- vs];
map[JSO,JSO] obj2object((Object)`{<{KeyValue ","}* kvs>}`) = (basic2jso(kv.key):basic2jso(kv.val) | KeyValue kv <- kvs);

//| jsarray(list[JSO] xs)
//	| jsobject(map[JSO,JSO] kvs)
//	| jsnull()

// 
// public void main(list[str] args)
// {
// 	loc src = |cwd:///|+args[0];
// 	PT = parse(#BasicType,src);
// 	println("Extraction completed.");
// }
