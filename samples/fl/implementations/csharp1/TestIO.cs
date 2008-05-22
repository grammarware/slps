using System.IO;
using System.Text;
using System.Xml.Serialization;

class MainClass
{
	static void Main(string[] args)
        {
            // Unmarshal program
            var reader1 = new StreamReader(args[0]);
            var serializer1 = new XmlSerializer(typeof(Program));
            var program = (Program)serializer1.Deserialize(reader1);

            // Pretty print program and save it in file
            var s = new StringBuilder();
            program.prettyPrint(s);
            var f = new StreamWriter(args[1]);
            f.Write(s.ToString());
            f.Close();
        }
}
