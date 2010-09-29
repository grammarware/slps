generate:
	@echo 'Generating the big test set...'
	@python ../../../shared/python/generateBGF.py tests
	@echo 'Generating the small test set...'
	@python ../../../shared/python/generateBGF.py tests.small etn qpk --nolabel
	@echo 'Both test cases generated.'
	
test:
	/usr/local/bin/xmllint --noout --schema ../../../shared/xsd/bgf.xsd tests/*

clean:
	rm -f tests/* tests.small/*