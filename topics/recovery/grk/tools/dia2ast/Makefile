no-all:

clean:	
	@rm -f *.ast
	@rm -f *~

test:	clean
	@make test1.ast
	@make test2.ast
	@make test3.ast
	@make test4.ast
	@make test5.ast
	@make test6.ast

%.ast:	%.dia
	pl -f main.pl -- "$<" "$@"
