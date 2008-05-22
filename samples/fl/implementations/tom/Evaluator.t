import java.util.*;
import fl.types.*;

public class Evaluator {

	%include { fl/FL.tom }

	/*
	* A local helper type for environments for expression evaluation
	*/
	private static class Environment {

		private Program program;
		private Stack<HashMap<String,Integer>> args;

		private Function lookupFunction(String n) {
		    Program p = program;
		    while (true) {
			%match(p) {	
				program() -> { return null; }
				program(f,F*) -> {
					if (`f.getname().equals(n))
						return `f;
					else 
						p = `F;
				} 
			}
		    }
		}

		private int lookupArgument(String n) {
			return args.peek().get(n);
		}

		private void bindArgs(HashMap<String,Integer> m, Formal f, Actual a) {
			%match(f) {
			   formal(fh,ft*) -> {
			      %match(a) {
				actual(ah,at*) -> {
						m.put(`fh,eExpr(this,`ah));
						bindArgs(m,`ft,`at);
					}
			      }
			   }
			} 
		}

		private int eApply(String n, Actual a) {
			Function f = lookupFunction(n);
			HashMap<String,Integer> m = new HashMap<String,Integer>();
			bindArgs(m,f.getargs(),a);
			args.push(m);
			int i = eExpr(this,f.getrhs());
			args.pop();
			return i;
		}
	}

	/**
	* Evaluate an expression with functions in scope
	**/
	public static int evaluate(Program p, Expr e) {
		Environment env = new Environment();
		env.program = p;
		env.args = new Stack<HashMap<String,Integer>>();
		return eExpr(env,e);
	}

	/**
	* Evaluate expression
	**/
	private static int eExpr(Environment m,Expr e) {
	    %match(e) {
		literal(i)        -> { return `i; }
		argument(n)       -> { return m.lookupArgument(`n); }
		binary(o,l,r)     -> { return eOps(`o,eExpr(m,`l),eExpr(m,`r)); }
		ifThenElse(x,y,z) -> { return eExpr(m,`x) != 0 ? eExpr(m,`y) : eExpr(m,`z); }
		apply(n,l)        -> { return m.eApply(`n,`l); }
	    }
	    throw new RuntimeException("Reached dead code.");
	}

	/**
	* Interpret operation symbols
	**/
	private static int eOps(Ops o, int x, int y) {
		%match(o) {
			equal() -> { return x == y ? -1 : 0; }
			plus()  -> { return x + y; }
			minus() -> { return x - y; }
		}
		throw new RuntimeException("Reached dead code.");
	}	
}