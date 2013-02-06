@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module extract::PyParsing

layout WS = [\ \t\n\r]* !>> [\ \t\n\r];
keyword PyPaKwd = "return" | "lambda" | "for" | "in"; //setParseAction
start syntax PyPaFile = PyPaFragment*;
//syntax PyPaFragment = PyPaImport | PyPaEquation | PyPaFunction | PyPaEmptyLine;
syntax PyPaFragment = PyPaEquation ";";
syntax PyPaImport = "import" Id ("as" Id)? "\n" | "from" Id "import" (Id | "*") "\n";
syntax PyPaFunction = "def" Id "(" {Id ","}* ")" PyPaFunctionBody;
syntax PyPaFunctionBody = PyPaLine* PyPaLastLine;  
syntax PyPaLine = Id PyPaUp2EOLn [\n];
syntax PyPaLastLine = "return" PyPaUp2EOLn [\n];
lexical PyPaEmptyLine = "\n";
lexical PyPaUp2EOLn = ![\n]* >> [\n];

syntax PyPaEquation = //[\n] << 
(PyPaLHS PyPaDef)? PyPaExpr2;

syntax PyPaExpr2
	= Id
	| QString
	| PyPaExpr2 "(" {PyPaExpr2 ","}* ")"
	| "lambda" Id ":" PyPaBalanced
	| "(" PyPaExpr2 ")"
	| "(" PyPaExpr2 "for" Id "in" PyPaExpr2 ")" 
	> left PyPaExpr2 PyPaOp PyPaExpr2 
	;

syntax PyPaOp = "." | "+" | "|" | "^";
syntax PyPaDef = "=" | "\<\<";

syntax PyPaBalanced = PyPaLambdaElement* () >> ")";
lexical PyPaLambdaElement
	= [(] PyPaBalanced [)]
	| ![()]
	; 

syntax PyPaLHS
	= {PyPaExpr2 ","}+
	;

lexical QString = [\"] QChars [\"];
lexical QChars = ![\"]* >> [\"];

lexical Id = [a-zA-Z_]+ !>> [a-zA-Z_] ; 
