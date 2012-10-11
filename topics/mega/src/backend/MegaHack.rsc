@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaHack

import structure::MegaADT;

str nameOf(artifact(_,_,_,_)) = "Artifact";
str nameOf(file(_,_,_,_)) = "File";
str nameOf(language(_,_,_,_)) = "Language";
str nameOf(technology(_,_,_,_)) = "Technology";
str nameOf(fragment(_,_,_,_)) = "Fragment";
str nameOf(objectGraph(_,_,_,_)) = "ObjectGraph";
str nameOf(program(_,_,_,_)) = "Program";
str nameOf(library(_,_,_,_)) = "Library";
str nameOf(function(_,_,_,_)) = "Function";
str nameOf(functionapp(_,_,_,_)) = "FunctionApp";
default str nameOf(MegaDeclaration d) = "UNKNOWN DECL";

str nameOf(subsetOf(_,_,_)) = "subsetOf";
str nameOf(elementOf(_,_,_)) = "elementOf";
str nameOf(partOf(_,_,_)) = "partOf";
str nameOf(correspondsTo(_,_,_)) = "correspondsTo";
str nameOf(dependsOn(_,_,_)) = "dependsOn";
str nameOf(refersTo(_,_,_)) = "refersTo";
str nameOf(conformsTo(_,_,_)) = "conformsTo";
str nameOf(realizationOf(_,_,_)) = "realizationOf";
str nameOf(descriptionOf(_,_,_)) = "descriptionOf";
str nameOf(definitionOf(_,_,_)) = "definitionOf";
str nameOf(inputOf(_,_,_)) = "inputOf";
str nameOf(hasOutput(_,_,_)) = "hasOutput";
str nameOf(domainOf(_,_,_)) = "domainOf";
str nameOf(hasRange(_,_,_)) = "hasRange";
default str nameOf(MegaRelation r) = "UNKNOWN REL";
