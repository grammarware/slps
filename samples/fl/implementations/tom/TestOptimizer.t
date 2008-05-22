import fl.types.*;
import org.antlr.runtime.*;
import java.io.*;

public class TestOptimizer {

    %include{ fl/FL.tom }
	
    public static void main(String[] args) throws Exception {

	// Parse unoptimized expression
        ANTLRFileStream input = new ANTLRFileStream(args[0]);
        FLLexer lexer = new FLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FLParser parser = new FLParser(tokens);
        Expr unoptimized = parser.expr();

	// Parse optimized expression (expected result)
	input = new ANTLRFileStream(args[1]);
	lexer = new FLLexer(input);
	tokens = new CommonTokenStream(lexer);
	parser = new FLParser(tokens);
	Expr expected = parser.expr();

	// Optimize program
	Expr actual = Optimizer.optimize(unoptimized);
	assert expected.equals(actual);
    }
}
