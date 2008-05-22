import fl.*;
import java.io.*;
import javax.xml.bind.*;

public class Test {
    public static void main (String[] args) throws Exception {

	// Unmarshal XML data into objects 
	JAXBContext jaxbContext = JAXBContext.newInstance("fl");
	Unmarshaller unMarshaller = jaxbContext.createUnmarshaller();
	Program program = (Program) unMarshaller.unmarshal(new File(args[0]));

	// Pretty print program and save it in file
	String s = program.prettyPrint();
	FileOutputStream output = new FileOutputStream (args[1]);
	new PrintStream(output).print(s);
	output.close();	

	// Evaluate program
	JAXBElement<Expr> expr =
	    (JAXBElement<Expr>) unMarshaller.unmarshal(new File(args[2]));
	assert Evaluator.evaluate(program, expr.getValue()) == 120;
    }
}
