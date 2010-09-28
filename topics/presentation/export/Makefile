all:
	cp ../../fl/txl/FL.Txl FL0.Txl
	txl2bgf FL0.Txl fl1.bgf
	bgf2txl fl1.bgf FL2.Txl
	txl2bgf FL2.Txl fl3.bgf
	bgf2bnf fl1.bgf fl1.bnf
	bgf2bnf fl3.bgf fl3.bnf
	diff fl1.bnf fl3.bnf

clean:
	rm -f *~
