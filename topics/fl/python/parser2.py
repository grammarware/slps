_Literal = Literal ;
defaultWhitespace = ParserElement.DEFAULT_WHITE_CHARS ;
ParserElement.setDefaultWhitespaceChars(ParserElement.DEFAULT_WHITE_CHARS.replace("\n", "")) ;

expr = Forward() ;

_IF, _THEN, _ELSE = (Keyword(i).suppress() for i in "if then else".split()) ;

name = NotAny(_IF | _THEN | _ELSE)+Word(
        alphas
        ).setParseAction(lambda tok: str(tok[0])) ;
literal = (
        Optional("-") + Word(nums)
        ).setParseAction(lambda tok: t.Literal("".join(tok))) ;
atom = name ^ literal ^ (Suppress("(") + expr + Suppress(")")) ;

ifThenElse = (
        _IF + expr + _THEN + expr + _ELSE + expr
        ).setParseAction(lambda tok: t.IfThenElse(*tok)) ;

operators = oneOf("== + -") ;

binary = (
        atom + ZeroOrMore((operators + atom).setParseAction(lambda tok: (tok[0], tok[1])))
        ).setParseAction(_lassoc) ;
apply = (
        name.copy() + Group(OneOrMore(atom))
        ).setParseAction(lambda tok: t.Apply(tok[0], *(i for i in tok[1]))) ;

expr << (binary ^ apply ^ ifThenElse) ;

function = (
        name.copy() + OneOrMore(name.copy().setParseAction(ArgumentCollector.append)) + Suppress("=") + expr
        ).setParseAction(lambda tok: t.Function(tok[0], ArgumentCollector.get(), tok[-1], ArgumentCollector.reset())) ;

name.setParseAction(ArgumentCollector.bind) ;

program = OneOrMore(
        function
        ).setParseAction(lambda tok: t.Program(list(tok))) + StringEnd().setWhitespaceChars(defaultWhitespace) ;