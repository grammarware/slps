all:
	make build
	make test

build:
	cp ../../../101repo/capabilities/Object_XML_mapping/MegaL/*.megal tests
	cp ../../../101repo/languages/MegaL/parser/megal/parser/MegaL.g .

test:
	cd src && java -Xmx1G -Xss32m -jar ../../../download/rascal.jar MegaParser.rsc
