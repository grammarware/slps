/*

HTML functionality for GRK
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam
(We don't need much HTML functionality.)

*/


% Remove all markup and ASCII encodiing from HTML
html2tt([],[]) :- !.

html2tt(S0,S2) :- 
  concat(["<",X,">",S1],S0),
  \+ member(0'<,X),
  !,
  html2tt(S1,S2).

html2tt(S0,[Y|S2]) :- 
  ampcode(X,Y),
  concat([X,S1],S0),
  !,
  html2tt(S1,S2).

html2tt([C|S0],[C|S1]) :-
  html2tt(S0,S1).


ampcode("&gt;",0'>).
ampcode("&lt;",0'<).
