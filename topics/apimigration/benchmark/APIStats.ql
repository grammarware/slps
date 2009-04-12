/**
 * @name APIStats
 * @kind table
 */
import default

from Class type, int featureCount, int LOC, string API
where 
(
 	API = "nu.xom" 
  and type.getPackage().getName() = "nu.xom" 
  
  and (
			type.getName() = "Attribute" or
			type.getName() = "Comment" or
			type.getName() = "DocType" or
			type.getName() = "Builder" or
			type.getName() = "Document" or
			type.getName() = "Element" or
			type.getName() = "Elements" or
			type.getName() = "Node" or
			type.getName() = "Nodes" or
			type.getName() = "ParentNode" or
			type.getName() = "ProcessingInstruction" or
			type.getName() = "Text" 
			)
  and LOC = type.getCompilationUnit().getNumberOfLinesOfCode()
  and featureCount = count(Method m | 
 				m.isPublic() and not m.hasModifier("abstract") 
				and (type.contains(m) or type.getASupertype().contains(m)) )
			+ count(Constructor cons | cons.isPublic() 
				and (type.contains(cons) or type.getASupertype().contains(cons))))
or ( 
	API = "org.jdom" 
	and (type.getPackage().getName() = "org.jdom" 
		or type.getPackage().getName() = "org.jdom.input"
		or type.getPackage().getName() = "org.jdom.output") 
  and (
			type.getName() = "Attribute" or
			type.getName() = "CDATA" or
			type.getName() = "SAXBuilder" or
			type.getName() = "XMLOutputter" or
			type.getName() = "Comment" or
			type.getName() = "DocType" or
			type.getName() = "Document" or
			type.getName() = "Element" or
			type.getName() = "Content" or
			type.getName() = "ProcessingInstruction" or
			type.getName() = "Namespace" or
			type.getName() = "EntityRef" or
			type.getName() = "Parent" or
			type.getName() = "Text" 
			)

  	and LOC = type.getCompilationUnit().getNumberOfLinesOfCode()
	  and featureCount = count(Method m | 
				m.isPublic() and not m.hasModifier("abstract") 
				and (type.contains(m) or type.getASupertype().contains(m)))
       + count(Constructor cons | cons.isPublic() 
				and (type.contains(cons) or type.getASupertype().contains(cons)))
 )

select API, type, LOC, featureCount