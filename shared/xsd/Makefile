validator = ../../download/msv/msv.jar
viewer = ../tools/showxsd

build:

test:

check:
	@for file in *.xsd ; \
	do \
		echo Validating $$file ... ; \
		java -jar ${validator} $$file ; \
		echo Translating $$file to BGF ... ; \
		${viewer} $$file > /dev/null ;\
	done

clean:
	rm -rf bin obj
	rm -f *.user
	rm -f *.suo

