import fl.types.*;

public class PrettyPrinter {

	%include { fl/FL.tom }

	/**
	* Pretty print functions of a program
	**/
	public static String prettyPrint(Program p) {
		%match(p) {
			program(f,F*) -> {
					return ppFunction(`f)
					      + "\n"
					      + prettyPrint(`F*); 
				} 
		}
		return "";
	}

	/**
	* Pretty print function 
	**/
	private static String ppFunction(Function f) {
		return f.getname()
			 + ppFormal(f.getargs())
			 + " = "
			 + ppExpr(f.getrhs());
	}

	/**
	* Pretty print formal arguments
	**/
	private static String ppFormal(Formal args) {
		%match(args) {
			formal(s,S*) -> {
				return " " + `s + ppFormal(`S*); 
			} 
		}
		return "";
	}

	/**
	* Pretty print expression
	**/
	private static String ppExpr(Expr e) {
		%match(e) {
			literal(i)        -> { return Integer.toString(`i); }
			argument(n)       -> { return `n; }
			binary(o,l,r)   -> { return "(" + ppExpr(`l) + ppOps(`o) + ppExpr(`r) + ")"; }
			ifThenElse(x,y,z) -> { return "if " + ppExpr(`x) + " then " + ppExpr(`y) + " else " + ppExpr(`z); }
			apply(n,l)        -> { return "(" + `n + ppActual(`l) + ")"; }
		}
		throw new RuntimeException("Reached dead code.");
	}

	/**
	* Pretty print operation symbol
        */
	private static String ppOps(Ops o) {
		%match(o) {
			equal() -> { return "=="; }
			plus() -> { return " + "; }
			minus() -> { return " - "; }
		}
		throw new RuntimeException("Reached dead code.");
	}

	/**
	* Pretty print actual arguments
	**/
	private static String ppActual(Actual args) {
		%match(args) {
			actual(e,E*) -> {
				return " " + ppExpr(`e) + ppActual(`E*); 
			} 
		}
		return "";
	}
}