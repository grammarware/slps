factorial = ../../repository/factorial.txt
temp = temp.txt

all: test

test: 
	@echo Testing implementation `pwd`
	@swipl -q -f Main.pro -t "main('${factorial}','${temp}')"
	@diff ${factorial} ${temp}
	@rm -f temp.txt
	@echo Done.

clean:
	rm -rf *~
	rm -f temp.txt
