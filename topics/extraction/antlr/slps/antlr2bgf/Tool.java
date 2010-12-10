package slps.antlr2bgf;

import java.io.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Tool
{
    public static void main(String args[]) throws Exception
    {
    	CharStream input = null;
    	input = new ANTLRFileStream(args[0]);
        StrippedANTLRLexer lex = new StrippedANTLRLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lex);
        StrippedANTLRParser g = new StrippedANTLRParser(tokens);
        g.output = args[1];
	    g.grammarDef();

    }
}

