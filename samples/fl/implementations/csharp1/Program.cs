using System.Diagnostics;
using System.IO;
using System.Text;
using System.Xml.Serialization;

namespace fl
{
    class MainClass
    {
        static void Main(string[] args)
        {
            // Unmarshal XML data into objects 
            var reader1 = new StreamReader(args[0]);
            var serializer1 = new XmlSerializer(typeof(Program));
            var program = (Program)serializer1.Deserialize(reader1);

            // Pretty print program and save it in file
            var s = new StringBuilder();
            program.prettyPrint(s);
            var f = new StreamWriter(args[1]);
            f.Write(s.ToString());
            f.Close();

            // Evaluate program
            var reader2 = new StreamReader(args[2]);
            var serializer2 = new XmlSerializer(typeof(Expr));
            var expr = (Expr)serializer2.Deserialize(reader2);
            Trace.Assert(program.evaluate(expr) == 120);
        }
    }
}
