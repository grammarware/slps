@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module language::Prodsig

data Footprint
	= fpnt()
	| fpopt()
	| fpplus()
	| fpstar()
	| fpmany(list[Footprint] fps)
	| fpempty()
	;

alias Signature = rel[str,Footprint];
alias NameMatch = rel[str,str,bool];
