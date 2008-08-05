validator = ../../download/msv/msv.jar
viewer = ../tools/showxsd

build:

test:
	make check

check:
	@make check1 arg=bgf.xsd
	@make check1 arg=xbgf.xsd
	@make check1 arg=xbgf.xsd
	@make check1 arg=btf.xsd
	@make check1 arg=ldf.xsd

check-old:
	@for file in *.xsd ; \
	do \
		echo Validating $$file ... ; \
		java -jar ${validator} $$file ; \
		echo Translating $$file to BGF ... ; \
		${viewer} $$file > /dev/null ;\
	done

check1:
	@echo Validating $$arg ...
	@java -jar ${validator} $$arg
	@echo Translating $$arg to BGF ...
	@${viewer} $$arg > /dev/null

clean:
	rm -rf bin obj
	rm -f *.user
	rm -f *.suo

