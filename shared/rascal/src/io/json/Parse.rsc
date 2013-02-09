@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::json::Parse

import language::JSON;
import IO;
import String;
import ParseTree;
import util::Math;

JSO str2jso(str s)
{
	PT = parse(#JSONBasicType,trim(s));
	return basic2jso(PT);
}
	
JSO basic2jso(JSONBasicType t)
{
	//if ((BasicType)`Number v` := PT)
	//println("numberr!");
	switch(t)
	{
		case (JSONBasicType)`<JSONNumber v>`: return str2num("<v>");
		case (JSONBasicType)`<JSONString v>`: return str2str("<v>");
		case (JSONBasicType)`<JSONBoolean v>`: return str2bool("<v>");
		case (JSONBasicType)`<JSONArray v>`:
			return jsarray(arr2array(v));
			//return jsnull();
		case (JSONBasicType)`<JSONObject v>`:
			return jsobject(obj2object(v));
			//return jsnull();
		case (JSONBasicType)`<JSONNull v>`: return jsnull();
		default:
			println("dunno");
	}
	return jsnull();
}

JSO str2num(str s) = jsnumber(toReal(s)); // TODO so that it recognizes ints? (not crucial)
JSO str2str(str s) = jsstring(s);
JSO str2bool("true") = jsboolean(true);
default JSO str2bool(str s) = jsboolean(false);

list[JSO] arr2array((JSONArray)`[<{JSONBasicType ","}* vs>]`) = [basic2jso(v) | JSONBasicType v <- vs];
map[JSO,JSO] obj2object((JSONObject)`{<{JSONKeyValue ","}* kvs>}`) = (basic2jso(kv.key):basic2jso(kv.val) | JSONKeyValue kv <- kvs);

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
