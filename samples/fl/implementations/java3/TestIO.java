import fl.*;
import java.io.*;
import javax.xml.bind.*;

public class TestIO {

    public static void main (String[] args) throws Exception {

	// Unmarshal program 
	JAXBContext jaxbContext = JAXBContext.newInstance("fl");
	Unmarshaller unMarshaller = jaxbContext.createUnmarshaller();
	Program program = (Program) unMarshaller.unmarshal(new File(args[0]));

	// Pretty print program and save it in file
	String s = program.prettyPrint();
	FileOutputStream output = new FileOutputStream (args[1]);
	new PrintStream(output).print(s);
	output.close();	
    }
}
