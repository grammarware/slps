import java.util.*;
import fl.*;

/**
 * We implement an evaluator for FL as a regular virtual method.
 * However, we do not touch the schema-derived classes to this end.
 * Instead, we add the virtual method by means of inter-type declarations.
 */

public aspect Evaluator {

	//
	// The evaluator maintains a list of functions and an argument stack.
	// Note the limitation implied by the use of static variables.
	// The code is non-reentrant.
	//
	private static List<Function> functions;
	private static Stack<HashMap<String,Integer>> stack;

	/**
	 * Evaluate an expression with functions in scope
	**/
	public static int evaluate(Program p, Expr e) {
		functions = p.getFunction();
		stack = new Stack<HashMap<String,Integer>>();
		return e.evaluate();
	}

	public abstract int Expr.evaluate();
		
	public int Literal.evaluate() {
		return getInfo();
	}

	public int Argument.evaluate() {
		return stack.peek().get(getName());
	}

	public int Binary.evaluate() {
	    int x = getLeft().evaluate();
	    int y = getRight().evaluate();
	    switch (getOps()) {
		case EQUAL :
			return (x==y ? -1 : 0);
		case PLUS :
			return x + y;
		case MINUS :
			return x - y;
		default :
			throw new RuntimeException("Reached dead code.");
	    }
	}

	public int IfThenElse.evaluate() {
		return getIfExpr().evaluate() != 0 ?
				  getThenExpr().evaluate()
				: getElseExpr().evaluate();
	}

	public int Apply.evaluate() {
	    Function f = null;
	    for (Function g : functions)
		if (g.getName().equals(getName())) {
			f = g;
			break;
		}
	    HashMap<String,Integer> map = new HashMap<String,Integer>();
	    int i = 0;
	    Iterator<String> j = f.getArg().iterator();
	    for (Expr e : getArg()) {
		map.put(j.next(),e.evaluate());
		i++;
	    }
	    stack.push(map);
	    int result = f.getRhs().evaluate();
	    stack.pop();
	    return result;
	}
}
