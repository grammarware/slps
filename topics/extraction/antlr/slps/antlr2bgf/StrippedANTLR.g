grammar StrippedANTLR;
options {
        output=AST;
        ASTLabelType=CommonTree;
}
@header {
package slps.antlr2bgf;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;
import java.io.FileOutputStream;
}
@lexer::header {
package slps.antlr2bgf;
}
@members {
    String output;
	Document doc;
	Element root;
	Element hook;
}

grammarDef
	:
	{
		try{
		DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		doc = docBuilder.newDocument();
		root = doc.createElement("bgf:grammar");
		root.setAttribute("xmlns:bgf", "http://planet-sl.org/bgf");
		doc.appendChild(root);
		}catch (Exception e){System.out.println(e);}
	}
    	'grammar' ID ';' NEWLINE rule+
	{
		try{
		Transformer trans = TransformerFactory.newInstance().newTransformer();
		trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.transform(new DOMSource(doc), new StreamResult(new FileOutputStream(output)));
		}catch (Exception e){System.out.println(e);}
	};
	
rule
	:
	{
		Element rule = doc.createElement("bgf:production");
		Element nonterminal  = doc.createElement("nonterminal");
		Element expr  = doc.createElement("bgf:expression");
	}
	ID ':' def=rhs ';' NEWLINE
	{
		nonterminal.appendChild(doc.createTextNode($ID.text));
		rule.appendChild(nonterminal);
		if($def.xml.getNodeName().compareTo("bgf:expression")==0)
			rule.appendChild($def.xml);
		else
			{
			expr.appendChild($def.xml);
			rule.appendChild(expr);
			}
		root.appendChild(rule);
	};

rhs returns [Node xml]
@init
{
 Element exprs = doc.createElement("choice");
}
	:    child=alternative
		{
			if($child.xml.getNodeName().compareTo("bgf:expression")==0)
				exprs.appendChild($child.xml);
			else
			{
				Element wrapper = doc.createElement("bgf:expression");
				wrapper.appendChild($child.xml);
				exprs.appendChild(wrapper);
			}
		}
	('|' more=alternative 
		{
			if($more.xml.getNodeName().compareTo("bgf:expression")==0)
				exprs.appendChild($more.xml);
			else
			{
				Element wrapper = doc.createElement("bgf:expression");
				wrapper.appendChild($more.xml);
				exprs.appendChild(wrapper);
			}
		}
	)*
	{
    	NodeList choice = exprs.getChildNodes();
	if(choice.getLength()==1)//only one expression
		$xml = choice.item(0);
	else//more choices
		$xml = exprs;
	};
	
alternative returns [Node xml]
@init
{
 Element exprs = doc.createElement("sequence");
}
	:
		(
		child=element
			{
			if($child.xml.getNodeName().compareTo("bgf:expression")==0)
			{// expression!
				exprs.appendChild($child.xml);
			}
			else
			{
				Element wrapper = doc.createElement("bgf:expression");
				wrapper.appendChild($child.xml);
				exprs.appendChild(wrapper);
			}
			}
		)+
	{
    	NodeList sequence = exprs.getChildNodes();
	if(sequence.getLength()==1)//only one expression
		$xml = sequence.item(0);
	else//more sequential elements
		$xml = exprs;
	}
	;
	
element returns [Node xml]
	: {boolean flag = false;} (ID '=')? name=ID (sign=suffix {flag = true;})?
		{
			if(flag)
			{
				Element ebnf = doc.createElement($sign.name);
			   	Element expr = doc.createElement("bgf:expression");
			   	Element atom = doc.createElement("nonterminal");
				atom.appendChild(doc.createTextNode($name.text));
				expr.appendChild(atom);
				ebnf.appendChild(expr);
				$xml = ebnf;
			}
			else
			{
				Element atom = doc.createElement("nonterminal");
				atom.appendChild(doc.createTextNode($name.text));
				$xml = atom;
			}
		}
	| quoted=STRING_LITERAL
		{
			String q = $quoted.text;
			Element atom = doc.createElement("terminal");
			if (q.endsWith("'")&&q.startsWith("'"))
				q = q.substring(1,q.length()-1);
			atom.appendChild(doc.createTextNode(q));
			$xml = atom;
		}
	| {boolean flag = false;}'(' child=rhs ')' (sign=suffix {flag = true;})?
		{
			if(flag)
			{
				Element ebnf = doc.createElement($sign.name);
//			    	if(child.xml.getNodeName().compareTo("bgf:expression")==0)
//			    	{
//				    	NodeList sequence = $child.xml.getChildNodes();
//					ebnf.appendChild(sequence.item(0));
//			    	}
//			    	else
					ebnf.appendChild($child.xml);
				$xml = ebnf;
				}
			else
			{
				$xml = $child.xml;
			}
		}
	;

suffix returns [String name]
	: '?' {$name = "optional";}
	| '*' {$name = "star";}
	| '+' {$name = "plus";}
	;
	
ID      : ('a'..'z'|'A'..'Z'|'0'..'9'|'_')+ ;
NEWLINE : '\r'? '\n' ;
WS      : (' '|'\t')+ {skip();} ;

STRING_LITERAL
	:	'\'' LITERAL_CHAR LITERAL_CHAR* '\''
	;
fragment
LITERAL_CHAR
	:	ESC
	|	~('\''|'\\')
	;
fragment
ESC	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	'>'
		|	'u' XDIGIT XDIGIT XDIGIT XDIGIT
		|	. // unknown, leave as it is
		)
	;

fragment
XDIGIT :
		'0' .. '9'
	|	'a' .. 'f'
	|	'A' .. 'F'
	;
