import fl.types.*;
import org.antlr.runtime.*;
import java.io.*;

public class TestEvaluator {

    %include{ fl/FL.tom }
	
    public static void main(String[] args) throws Exception {

	// Parse file to program
        ANTLRFileStream input = new ANTLRFileStream(args[0]);
        FLLexer lexer = new FLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FLParser parser = new FLParser(tokens);
        Program program = parser.program();

	// Parse sample expression
        input = new ANTLRFileStream(args[1]);
        lexer = new FLLexer(input);
        tokens = new CommonTokenStream(lexer);
        parser = new FLParser(tokens);
        Expr expr = parser.expr();

	// Evaluate program
	int expected = Integer.parseInt(args[2]);
	assert expected == Evaluator.evaluate(program,expr);
    }
}
