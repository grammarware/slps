using System.Diagnostics;
using System.IO;
using System.Xml.Serialization;

namespace fl
{
    class MainClass
    {
        static void Main(string[] args)
        {
            // Unmarshal program
            var reader1 = new StreamReader(args[0]);
            var serializer1 = new XmlSerializer(typeof(Program));
            var program = (Program)serializer1.Deserialize(reader1);

            // Unmarshall expression
            var reader2 = new StreamReader(args[1]);
            var serializer2 = new XmlSerializer(typeof(Expr));
            var expr = (Expr)serializer2.Deserialize(reader2);

            // Evaluate program
	    var expected = System.Int32.Parse(args[2]);
            Trace.Assert(expected == program.evaluate(expr));
        }
    }
}
