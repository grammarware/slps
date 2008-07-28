import fl.types.*;
import java.util.LinkedList;

public class Factory {

	%include { fl/FL.tom }

	public static Program program(LinkedList<Function> functions) {
		Program p = `program();
		for (Function f : functions)
			p = `program(p*,f);
		return p;
	}

	public static Function function(
		String name, LinkedList<String> args, Expr rhs) 
	{
		Formal f = formal(args);
		return `function(name,f,rhs);
	}

	private static Formal formal(LinkedList<String> args) {
		Formal f = `formal();
		for (String s : args)
			f = `formal(f*,s);
		return f;
	}

	public static Expr literal(int info) {
		return `literal(info);
	}

	public static Expr argument(String name) {
		return `argument(name);
	}

	public static Expr binary(Ops ops, Expr left, Expr right) {
		return `binary(ops,left,right);
	}

	public static Expr ifThenElse(Expr cond, Expr ifTrue, Expr ifFalse) {
		return `ifThenElse(cond,ifTrue,ifFalse);
	}

	public static Expr apply(String name, LinkedList<Expr> args) {
		Actual a = actual(args);
		return `apply(name,a);
	}

	private static Actual actual(LinkedList<Expr> args) {
		Actual a = `actual();
		for (Expr e : args)
			a = `actual(a*,e);
		return a;
	}
	
	public static Ops equal() {
		return `equal();
	}

	public static Ops plus() {
		return `plus();
	}

	public static Ops minus() {
		return `minus();
	}
}