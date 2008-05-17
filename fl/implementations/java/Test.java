import org.antlr.runtime.*;
import java.util.*;
import java.io.*;

public class Test {
    public static void main(String[] args) throws Exception {

	// Parse file to program
        ANTLRFileStream input = new ANTLRFileStream(args[0]);
        TFPLLexer lexer = new TFPLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        TFPLParser parser = new TFPLParser(tokens);
        Program program = parser.program();

	// Pretty print program and save it in file
	PrettyPrinter pp = new PrettyPrinter();
	pp.visit(program);
	FileOutputStream output = new FileOutputStream (args[1]);
	new PrintStream(output).print(pp.getResult());
	output.close();	

	// Evaluate program
	Evaluator eval = new Evaluator(program);
	Apply apply = new Apply("fac", new LinkedList<Expr>());
	apply.args.add(new Literal(5));
	assert eval.evaluate(apply) == 120;
    }
}
