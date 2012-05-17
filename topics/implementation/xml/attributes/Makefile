test:
	swipl -f RunParser.pro -t "main('input.txt')"

fail:
	cat bad.txt
	swipl -f RunParser.pro -t "main('bad.txt')"

debug:
	cat input.txt
	swipl -s Parser.pro

clean:
	rm -rf *~
