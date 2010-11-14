test:
	swipl -f semantics.pro -t "eval(`cat input1.txt`,V,T),write(V),write(':'),write(T),nl"
	swipl -f semantics.pro -t "eval(`cat input2.txt`,V,T),write(V),write(':'),write(T),nl"

types:
	swipl -f types.pro -t "type(`cat input1.txt`,T),write(T),nl"
	swipl -f types.pro -t "type(`cat input2.txt`,T),write(T),nl"

prompt:
	swipl -s types.pro

