% -*- prolog -*-

:- set_prolog_flag(double_quotes, string).


parse_trace_from_file(Filename, Doc)
:-
	trace_from_file(Filename, Trace),
	parse_trace(Trace, Doc),
	nl, nl, write('Doc = '), write(Doc), nl.

parse_trace(Trace, Doc)
 :-
    build(Doc,Trace,[]).


% Utils

list_trace([]).
list_trace([Event|Tail]) :- writeq(Event), nl, list_trace(Tail).


trace_from_file(Filename, Trace) :-
	 open(Filename, read, Stream, []), read(Stream, Trace), 
	 list_trace(Trace),
	 close(Stream).

% Sequential implementation

find(Event, [Event|Tail], Tail).
%	Event \= (_LineNumber, _Event).
%	write('Finding: '), writeq(Event), nl.

