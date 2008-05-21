using System.IO;
using System.Text;
using System.Xml.Linq;

namespace fl
{
    class Program
    {
        static void Main(string[] args)
        {
            var program = XElement.Load(args[0]);
            var s = new StringBuilder();
            PrettyPrinter.prettyPrintProgram(program, s);
            var f = new StreamWriter(args[1]);
            f.Write(s.ToString());
            f.Close();
        }
    }
}
