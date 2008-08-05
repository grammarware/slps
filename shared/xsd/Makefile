validator = ../../download/msv/msv.jar
viewer = ../tools/showxsd

build:

test:

check:
	ls -1 *.xsd | xargs -n1 java -jar ${validator}
	ls -1 *.xsd | xargs -n1 ${viewer}

clean:
	rm -rf bin obj
	rm -f *.user
	rm -f *.suo

