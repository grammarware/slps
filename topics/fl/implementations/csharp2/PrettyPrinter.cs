using System.Text;
using System.Xml.Linq;

public static class PrettyPrinter
{
    static XNamespace tns = "fl";
    static XNamespace xsi = "http://www.w3.org/2001/XMLSchema-instance";
 
    public static void prettyPrintProgram(XElement x, StringBuilder s)
    {
        foreach (var y in x.Elements(tns + "function"))
        {
            s.Append((string)y.Element(tns + "name"));
            foreach (var a in y.Elements(tns + "arg"))
            {
                s.Append(" ");
                s.Append((string)a);
            }
            s.Append(" = ");
            prettyPrintExpression(y.Element(tns + "rhs"), s);
            s.Append("\n");
        }
    }

    public static void prettyPrintExpression(XElement x, StringBuilder s)
    {
        var type = (string)x.Attribute(xsi + "type");
        if (type == "Literal")
        {
            s.Append((int)x.Element(tns + "info"));
            return;
        }
        if (type == "Argument")
        {
            s.Append((string)x.Element(tns + "name"));
            return;
        }
        if (type == "Binary")
        {
            s.Append("(");
            prettyPrintExpression(x.Element(tns + "left"),s);
            var ops = (string)x.Element(tns + "ops");
            s.Append(ops == "Equal" ? "==" : ops == "Plus" ? " + " : " - ");
            prettyPrintExpression(x.Element(tns + "right"),s);
            s.Append(")");
            return;
        }
        if (type == "IfThenElse")
        {
            s.Append("if ");
            prettyPrintExpression(x.Element(tns + "ifExpr"),s);
            s.Append(" then ");
            prettyPrintExpression(x.Element(tns + "thenExpr"),s);
            s.Append(" else ");
            prettyPrintExpression(x.Element(tns + "elseExpr"),s);
            return;
        }
        if (type == "Apply")
        {
            s.Append("(");
            s.Append((string)x.Element(tns + "name"));
            foreach (var a in x.Elements(tns + "arg"))
            {
                s.Append(" ");
                prettyPrintExpression(a, s);
            }
            s.Append(")");
            return;
        }
    }
}
