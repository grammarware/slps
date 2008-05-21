/**
 * We implement a pretty printer for TFPL as a regular virtual method.
 * However, we do not touch the TFPL classes to this end.
 * Instead, we add the virtual method by means of inter-type declarations.
 */
public aspect PrettyPrinter {

	//
	// The use of an AGGREGATING string variable is potentially very inefficient.
	//
	
	public String Function.prettyPrint() {
		String result = "";
		result += name;
		for (String s : args) {
			result += " " + s;
		}
		result += " = ";
		result += rhs.prettyPrint();
		return result;
	}

	public abstract String Expr.prettyPrint();

	public String Literal.prettyPrint() {
		return Integer.toString(info);
	}
	
	public String Argument.prettyPrint() {
		return name;
	}
	
	public String Equality.prettyPrint() {
		return "("
			+ left.prettyPrint()
			+ "=="
			+ right.prettyPrint()
			+ ")";
	}
	
	public String Plus.prettyPrint() {
		return "("
			+ left.prettyPrint()
			+ " + "
			+ right.prettyPrint()
			+ ")";
	}
	
	public String Minus.prettyPrint() {
		return "("
			+ left.prettyPrint()
			+ " - "
			+ right.prettyPrint()
			+ ")";
	}
	
	public String Apply.prettyPrint() {
		String result = "";
		result += "(" + name;
		for (Expr e : args) {
			result += " " + e.prettyPrint();
		}
		result += ")";
		return result;
	}
	
	public String IfThenElse.prettyPrint() {
		return "if "
			+ exprIf.prettyPrint()
			+ " then "
			+ exprThen.prettyPrint()
			+ " else "
			+ exprElse.prettyPrint();
	}		
}
