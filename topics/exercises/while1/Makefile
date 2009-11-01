all: 
	swipl -f RunParser.pro -t "main('input.txt')"

test:
	cat input.txt
	swipl -s Parser.pro

clean:
	rm -rf *~
