import fl.*;

/**
 * We implement a pretty printer for FL as a regular virtual method.
 * However, we do not touch the schema-derived classes to this end.
 * Instead, we add the virtual method by means of inter-type declarations.
 */

public aspect PrettyPrinter {

	public String Program.prettyPrint() {
		String result = "";
		for (Function f : getFunction())
			result += f.prettyPrint() + "\n";
		return result;
	}
	
	public String Function.prettyPrint() {
		String result = "";
		result += getName();
		for (String s : getArg())
			result += " " + s;
		result += " = ";
		result += getRhs().prettyPrint();
		return result;
	}

	public abstract String Expr.prettyPrint();

	public String Literal.prettyPrint() {
		return Integer.toString(getInfo());
	}
	
	public String Argument.prettyPrint() {
		return getName();
	}
	
	public String Binary.prettyPrint() {
		return "("
			+ getLeft().prettyPrint()
			+ ( 
			    getOps() == Ops.EQUAL ?
			    "==" :
			    getOps() == Ops.PLUS ?
			    " + " :
			    " - "
			  ) 
			+ getRight().prettyPrint()
			+ ")";
	}
	
	public String IfThenElse.prettyPrint() {
		return "if "
			+ getIfExpr().prettyPrint()
			+ " then "
			+ getThenExpr().prettyPrint()
			+ " else "
			+ getElseExpr().prettyPrint();
	}		

	public String Apply.prettyPrint() {
		String result = "";
		result += "(" + getName();
		for (Expr e : getArg()) {
			result += " " + e.prettyPrint();
		}
		result += ")";
		return result;
	}
}
