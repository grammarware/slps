// An evaluator for FL
// It is defined as a partial-class-based extension of the schema-derived classes.
// We use a string builder instead of naive concatenation.

using System;
using System.Collections;
using System.Collections.Generic;

public partial class Program {

	//
	// The evaluator maintains a list of functions and an argument stack.
	// Note the limitation implied by the use of static variables.
	// The code is non-reentrant.
	//
	public static Function[] functions;
	public static Stack<Dictionary<string,int>> stack;

	/**
	 * Evaluate an expression with functions in scope
	**/
	public int evaluate(Expr e) {
		functions = function;
		stack = new Stack<Dictionary<string,int>>();
		return e.evaluate();
	}
}

public partial class Expr 
{
	public abstract int evaluate();
}

public partial class Literal
{
    public override int evaluate()
    {
		return info;
	}
}

public partial class Argument
{
    public override int evaluate()
    {
        int value;
        Program.stack.Peek().TryGetValue(name, out value);
        return value;
	}
}

public partial class Binary
{
    public override int evaluate()
    {
        int x = left.evaluate();
        int y = right.evaluate();
        switch (ops)
        {
            case Ops.Equal :
                return (x == y ? -1 : 0);
            case Ops.Plus :
                return x + y;
            case Ops.Minus :
                return x - y;
            default:
                throw new Exception("Reached dead code.");
        }
    }
}

public partial class IfThenElse
{
    public override int evaluate()
    {
		return ifExpr.evaluate() != 0 ?
				  thenExpr.evaluate()
				: elseExpr.evaluate();
	}
}

public partial class Apply
{
	public override int evaluate() {
	    Function f = null;
	    foreach (var g in Program.functions)
		if (g.name.Equals(name)) {
			f = g;
			break;
		}
	    Dictionary<string,int> map = new Dictionary<string,int>();
	    int i = 0;
	    var j = f.arg.GetEnumerator();
	    foreach (var e in arg) {
            j.MoveNext();
		    map.Add((string)j.Current,e.evaluate());
		    i++;
	    }
	    Program.stack.Push(map);
	    int result = f.rhs.evaluate();
        Program.stack.Pop();
        return result;
	}
}
