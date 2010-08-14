generate:
	python ../../../shared/python/generateBGF.py tests
	
test:
	/usr/local/bin/xmllint --noout --schema ../../../shared/xsd/bgf.xsd tests/*

clean:
	rm -f tests/*