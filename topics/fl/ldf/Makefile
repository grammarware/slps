all:
	java -jar ../../../download/msv/msv.jar ../../../shared/xsd/ldf.xsd fl.ldf
	../../../shared/tools/ldf2bgf fl.ldf fl.bgf
	java -jar ../../../download/msv/msv.jar ../../../shared/xsd/bgf.xsd fl.bgf

clean:
	rm -f fl.bgf

