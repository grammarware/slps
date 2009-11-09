test:
	swipl -f RunParser.pro -t "main('input.txt')"

fail:
	swipl -f RunParser.pro -t "main('bad.txt')"

debug:
	cat input.txt
	swipl -s Parser.pro

clean:
	rm -rf *~
