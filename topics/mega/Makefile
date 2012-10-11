all:
	make test
	cd tests && make all

test:
	cd src && java -Xmx1G -Xss32m -jar ../../../download/rascal.jar MegaParser.rsc

clean:
	rm -f tests/*.dot tests/*.pdf tests/*.megal
