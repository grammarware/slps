grammar FL;

@header {
import fl.types.*;
import java.util.LinkedList;
}

program returns [Program program] : 
        { LinkedList<Function> functions = new LinkedList<Function>(); }
        ( f=function { functions.add($f.function); } )+
        { $program = Factory.program(functions); }
        ;

function returns [Function function] :
        { LinkedList<String> args = new LinkedList<String>(); }
        n=ID
        ( a=ID { args.add($a.text); } )+
        '='
        e=expr NEWLINE
        { $function = Factory.function($n.text,args,$e.expr); }
        ;

expr returns [Expr expr] :
	  b=binary { $expr = $b.expr; }
	| a=apply { $expr = $a.expr; }
 	| i=ifthenelse { $expr = $i.expr; }
      ;

binary returns [Expr expr] :
        l=atom { $expr = $l.expr; } 
        ( o=ops r=atom { $expr = Factory.binary($o.symbol,$expr,$r.expr); } )*
        ;

apply returns [Expr expr] :
        { LinkedList<Expr> args = new LinkedList<Expr>(); }
        i=ID
        ( a=atom { args.add($a.expr); } )+
        { $expr = Factory.apply($i.text,args); } 
        ;

ifthenelse returns [Expr expr] :
        'if' c=expr 'then' e1=expr 'else' e2=expr
        { $expr = Factory.ifThenElse($c.expr,$e1.expr,$e2.expr); }
        ;

atom returns [Expr expr] :
	  ID { $expr = Factory.argument($ID.text); }
	| INT { $expr = Factory.literal(Integer.parseInt($INT.text)); }
	| '(' e=expr ')' { $expr = $e.expr; }
 	;

ops returns [Ops symbol] :
	  '==' { $symbol = Factory.equal(); }
	| '+' { $symbol = Factory.plus(); }
	| '-' { $symbol = Factory.minus(); }
	;

ID  	: ('a'..'z')+ ;
INT 	: '-'?'0'..'9'+ ;
NEWLINE	: '\r'? '\n' ;
WS  	: (' '|'\t')+ {skip();} ;
