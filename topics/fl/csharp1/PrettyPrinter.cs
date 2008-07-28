// A pretty printer for FL
// It is defined as a partial-class-based extension of the schema-derived classes.
// We use a string builder instead of naive concatenation.

using System.Text;

public partial class Program
{
    public void prettyPrint(StringBuilder s)
    {
        foreach (var f in function)
        {
            f.prettyPrint(s);
            s.Append("\n");
        }
    }
}

public partial class Function
{
    public void prettyPrint(StringBuilder s)
    {
        s.Append(name);
        foreach (var a in arg)
            s.Append(" " + a);
        s.Append(" = ");
        rhs.prettyPrint(s);
    }
}

public abstract partial class Expr
{
    public abstract void prettyPrint(StringBuilder s);
}

public partial class Literal
{
    public override void prettyPrint(StringBuilder s)
    {
        s.Append(info);
    }
}

public partial class Argument
{
    public override void prettyPrint(StringBuilder s)
    {
        s.Append(name);
    }
}

public partial class Binary
{
    public override void prettyPrint(StringBuilder s)
    {
        s.Append("(");
        left.prettyPrint(s);
        switch (ops)
        {
            case Ops.Equal: s.Append("=="); break;
            case Ops.Plus: s.Append(" + "); break;
            case Ops.Minus: s.Append(" - "); break;
        }
        right.prettyPrint(s);
        s.Append(")");
    }
}

public partial class IfThenElse
{
    public override void prettyPrint(StringBuilder s)
    {
        s.Append("if ");
        ifExpr.prettyPrint(s);
        s.Append(" then ");
        thenExpr.prettyPrint(s);
        s.Append(" else ");
        elseExpr.prettyPrint(s);
    }
}

public partial class Apply
{
    public override void prettyPrint(StringBuilder s)
    {
        s.Append("(");
        s.Append(name);
        foreach (var a in arg)
        {
            s.Append(" ");
            a.prettyPrint(s);
        }
        s.Append(")");
    }
}
