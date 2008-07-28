import fl.*;
import java.io.*;
import javax.xml.bind.*;

public class TestEvaluator {

    public static void main (String[] args) throws Exception {

	// Unmarshal program 
	JAXBContext jaxbContext = JAXBContext.newInstance("fl");
	Unmarshaller unMarshaller = jaxbContext.createUnmarshaller();
	Program program = (Program) unMarshaller.unmarshal(new File(args[0]));

	// Unmarshall expression
	JAXBElement<Expr> expr =
	    (JAXBElement<Expr>) unMarshaller.unmarshal(new File(args[1]));

	// Evaluate program
	int expected = Integer.parseInt(args[2]);
	assert expected == Evaluator.evaluate(program,expr.getValue());
    }
}
