generate:
	@echo 'Generating the big test set...'
	@rm -f tests/*
	@rm -f tests.draft/*
	@rm -f tests.small/*
	@python ../../../shared/python/generateBGF.py tests.draft
	@echo 'Normalizing the big test set...'
	@ls -1 tests.draft | xargs -n1 -I _ xbgf ../../transformation/normalization/empty.xbgf tests.draft/_ tests/_
	@rm -f tests.draft/*
	@echo 'Generating the small test set...'
	@python ../../../shared/python/generateBGF.py tests.draft etn qpk --nolabel
	@echo 'Normalizing the small test set...'
	@ls -1 tests.draft | xargs -n1 -I _ xbgf ../../transformation/normalization/empty.xbgf tests.draft/_ tests.small/_
	@rm -f tests.draft/*
	@echo 'Both test cases generated.'
	
test:
	/usr/local/bin/xmllint --noout --schema ../../../shared/xsd/bgf.xsd tests/*

clean:
	rm -f tests/* tests.small/*