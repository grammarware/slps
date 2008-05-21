import java.util.HashMap;
import java.util.LinkedList;
import java.util.Stack;

/**
 * We implement an evaluator for TFPL as a regular virtual method.
 * However, we do not touch the TFPL classes to this end.
 * Instead, we add the virtual method by means of inter-type declarations.
 * 
 * The evaluator maintains a list of functions and an argument stack.
 */

public aspect Evaluator {

	//
	// Note the limitation implied by the use of static variables.
	// The code is non-reentrant.
	//
	private static LinkedList<Function> functions;
	private static Stack<HashMap<String,Integer>> stack;

	/**
	 * Evaluate an expression with functions in scope
	**/
	public static int evaluate(LinkedList<Function> fs, Expr e) {
			functions = fs;
			stack = new Stack<HashMap<String,Integer>>();
			return e.evaluate();
	}

	public abstract int Expr.evaluate();
		
	public int Literal.evaluate() {
		return info;
	}

	public int Argument.evaluate() {
		return stack.peek().get(name);
	}

	public int Equality.evaluate() {
		return (left.evaluate() == right.evaluate() ? -1 : 0);
	}
		
	public int Plus.evaluate() {
		return left.evaluate() + right.evaluate();
	}

	public int Minus.evaluate() {
		return left.evaluate() - right.evaluate();
	}

	public int IfThenElse.evaluate() {
		return exprIf.evaluate() != 0 ?
				  exprThen.evaluate()
				: exprElse.evaluate();
	}

	public int Apply.evaluate() {
		Function f = null;
		for (Function g : functions)
			if (g.name==name) {
				f = g;
				break;
			}
		HashMap<String,Integer> map = new HashMap<String,Integer>();
		int i = 0;
		for (Expr e : args) {
				map.put(f.args[i],e.evaluate());
				i++;
		}
		stack.push(map);
		int result = f.rhs.evaluate();
		stack.pop();
		return result;
	}
}
