validator = ../../download/msv/msv.jar
viewer = ../tools/showxsd

build:

test:

check:
	@echo Attempting validation...
	ls -1 *.xsd | xargs -n1 java -jar ${validator}
	@echo Attempting presentation...
	ls -1 *.xsd | xargs -n1 ${viewer} > /dev/null
	@echo All Ok

clean:
	rm -rf bin obj
	rm -f *.user
	rm -f *.suo

