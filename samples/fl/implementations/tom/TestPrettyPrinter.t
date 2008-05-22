import fl.types.*;
import org.antlr.runtime.*;
import java.io.*;

public class TestPrettyPrinter {

    %include{ fl/FL.tom }
	
    public static void main(String[] args) throws Exception {

	// Parse file to program
        ANTLRFileStream input = new ANTLRFileStream(args[0]);
        FLLexer lexer = new FLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FLParser parser = new FLParser(tokens);
        Program program = parser.program();

	// Pretty print program and save it in file
	String str = PrettyPrinter.prettyPrint(program);
	FileOutputStream output = new FileOutputStream (args[1]);
	new PrintStream(output).print(str);
	output.close();	
    }
}
