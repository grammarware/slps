import types.*;

/**
 * A visitor for pretty printing
 */
public class PrettyPrinter extends Visitor {
	//
	// An AGGREGATING string variable is potentially very inefficient.
	//
	private String result = "";
	public String getResult() {
		return result;
	}
	public void visit(Program x) {
		for (Function f : x.functions) {
			f.accept(this);
			result += "\n";
		}
	}
	public void visit(Function x) {
		result += x.name;
		for (String s : x.args) {
			result += " " + s;
		}
		result += " = ";
		x.rhs.accept(this);
	}
	public void visit(Literal x) {
		result += x.info;
	}
	public void visit(Argument x) {
		result += x.name;
	}
	public void visit(Binary x) {
		result += "(";
		x.left.accept(this);
		switch (x.ops) {
		    case Equal :
			result += "==";
			break;
		    case Plus :
			result += " + ";
			break;
		    case Minus :
			result += " - ";
			break;
		}
		x.right.accept(this);		
		result += ")";
	}
	public void visit(Apply x) {
		result += "(" + x.name;
		for (Expr e : x.args) {
			result += " ";
			e.accept(this);
		}
		result += ")";
	}
	public void visit(IfThenElse x) {
		result += "if ";
		x.exprIf.accept(this);
		result += " then ";
		x.exprThen.accept(this);
		result += " else ";
		x.exprElse.accept(this);
	}
}
