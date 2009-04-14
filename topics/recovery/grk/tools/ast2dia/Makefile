no-all:

clean:	
	@rm -f *.ast
	@rm -f *.pp
	@rm -f *~

copy:   clean
	cd ../dia2ast; gmake test
	cp ../dia2ast/*.ast .

test:	clean copy
	make test1.pp
	make test2.pp
	make test3.pp
	make test4.pp
	make test5.pp
	make test6.pp

%.pp:	%.ast
	pl -f main.pl -- "$<" "$@"
	more $@
