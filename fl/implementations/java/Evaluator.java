import java.util.*;

/**
 * A visitor for evaluation
 */
public class Evaluator extends Visitor {
	private int result;
	private LinkedList<Function> functions;
	private Stack<HashMap<String,Integer>> stack;
	public Evaluator(Program program) {
	    functions = program.functions;
	    stack = new Stack<HashMap<String,Integer>>();
	}
	public int evaluate(Expr e) {
	    e.accept(this);
	    return result;
	}
	public void visit(Program x) {
	    throw new EvaluatorException();
	}
	public void visit(Function x) {
	    throw new EvaluatorException();
	}
	public void visit(Literal x) {
	    result = x.info;
	}
	public void visit(Argument x) {
	    result = stack.peek().get(x.name);
	}
	public void visit(Binary x) {
	    int a = evaluate(x.left);
	    int b = evaluate(x.right);
	    switch (x.ops) {
	       case Equal : 
		   result = a == b ? -1 : 0;
		   break;
	       case Plus : 
		   result = a + b;
		   break;
	       case Minus : 
		   result = a - b;
		   break;
	    }
	}
	public void visit(IfThenElse x) {
	    result = evaluate(x.exprIf) != 0 ?
		  evaluate(x.exprThen)
		: evaluate(x.exprElse);
	}
	public void visit(Apply x) {
	    Function f = null;
	    for (Function g : functions)
		if (g.name.equals(x.name)) {
		    f = g;
		    break;
		}
	    HashMap<String,Integer> map = new HashMap<String,Integer>();
	    Iterator<String> i = f.args.iterator();
	    for (Expr e : x.args)
		map.put(i.next(),evaluate(e));
	    stack.push(map);
	    f.rhs.accept(this);
	    stack.pop();
	}
}
