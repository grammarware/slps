grammar FL;
options {backtrack=true;}

@header {
import types.*;
import java.util.LinkedList;
}

program returns [Program program] : 
        { $program = new Program(new LinkedList<Function>()); }
        ( f=function { $program.functions.add($f.function); } )+
        ;
    
function returns [Function function] :
        n=ID
        { $function = new Function($n.text,new LinkedList<String>(), null); }
        ( a=ID { $function.args.add($a.text); } )+
        '='
        e=expr NEWLINE
        { $function.rhs = $e.expr; }
        ;

expr returns [Expr expr] :
        b=binary { $expr = $b.expr; }
      | a=apply { $expr = $a.expr; }
 	  | i=ifthenelse { $expr = $i.expr; }
      ;

binary returns [Expr expr] :
        l=atom { $expr = $l.expr; } 
        ( o=ops r=atom { $expr = new Binary($o.symbol,$expr,$r.expr); } )*
        ;

apply returns [Apply expr] :
        i=ID
        { $expr = new Apply($i.text, new LinkedList<Expr>()); } 
        ( a=atom { $expr.args.add($a.expr); } )+
        ;

ifthenelse returns [IfThenElse expr] :
        'if' c=expr 'then' e1=expr 'else' e2=expr
        { $expr = new IfThenElse($c.expr,$e1.expr,$e2.expr); }
        ;

atom returns [Expr expr] :
      ID { $expr = new Argument($ID.text); }
    | INT { $expr = new Literal(Integer.parseInt($INT.text)); }
	| '(' e=expr ')' { $expr = $e.expr; }
 	;

ops returns [Ops symbol] :
        '==' { $symbol = Ops.Equal; }
        |'+' { $symbol = Ops.Plus; }
        |'-' { $symbol = Ops.Minus; }
        ;

ID  	:	('a'..'z')+ ;
INT 	:	'-'?'0'..'9'+ ;
NEWLINE	:	'\r'? '\n' ;
WS  	:	(' '|'\t')+ {skip();} ;
