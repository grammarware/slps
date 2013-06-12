@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Naming

import language::BGF;

public set[str] camelcases2(SGrammar g) = {n | str n <- g.prods,
/^([A-Z][a-z\d\s\_\/\-\:\.]+)+$/ := n};
public set[str] mixedcases2(SGrammar g) = {n | str n <- g.prods,
/^[a-z]+([A-Z\d\s\_\/\-\:\.][a-z\d\s\_\/\-\:\.]*)$/ := n};
public set[str] lowercases2(SGrammar g) = {n | str n <- g.prods,
/^[a-z\d\s\_\/\-\:\.]+$/ := n};
public set[str] uppercases2(SGrammar g) = {n | str n <- g.prods,
/^[A-Z\d\s\_\/\-\:\.]+$/ := n};

public set[str] camelcases1(SGrammar g) = {n | str n <- g.prods,
/^([A-Z][a-z]*)+$/ := n};
public set[str] mixedcases1(SGrammar g) = {n | str n <- g.prods,
/^[a-z]+([A-Z][a-z]*)+$/ := n};
public set[str] lowercases1(SGrammar g) = {n | str n <- g.prods,
/^[a-z]+$/ := n};
public set[str] uppercases1(SGrammar g) = {n | str n <- g.prods,
/^[A-Z]+$/ := n};

// The next one is a bit more tricky. What we want, is:
//  - more than one word
//  - camelcase, mixedcase or separated
//  - separators can be spaces, dashes, slashes and underscores
public set[str] multiwords2(SGrammar g) = {n | str n <- g.prods,
/^\w[a-z0-9_]*([A-Z0-9_][a-z0-9_]*)+$/ := n ||
/^\w+([\s_\/\-\:\.]\w+)+$/ := n};
public set[str] multiwords1(SGrammar g) = {n | str n <- g.prods,
/^[a-zA-Z][a-z]*([A-Z][a-z]*)+$/ := n ||
/^[a-zA-Z]+([\s_\/\-\:\.][a-zA-Z]+)+$/ := n};

public map[str name,set[str](SGrammar) fun] NamingPatterns =
	(
		"CamelCase":			camelcases1,			// CamelCase
		"MixedCase":			mixedcases1,			// mixedCase
		"LowerCase":			lowercases1,			// lowercase
		"UpperCase":			uppercases1,			// UPPERCASE
		"MultiWord":			multiwords1,			// Multiple_Words (includes camelcases and mixedcases with two or more words)
		"CamelCaseLax":			camelcases2,			// CamelCase
		"MixedCaseLax":			mixedcases2,			// mixedCase
		"LowerCaseLax":			lowercases2,			// lowercase
		"UpperCaseLax":			uppercases2,			// UPPERCASE
		"MultiWordLax":			multiwords2				// Multiple_Words (includes camelcases and mixedcases with two or more words)
	);
