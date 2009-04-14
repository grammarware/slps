/*

I/O functionality for GRK
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam
(We don't need much I/O functionality.)

*/


/* Character classes */

isSpace(0' ).
isUnderscore(0'_).
isHyphen(0'-).
isDoubleQuote(0'").
isNotDoubleQuote(0'") :- !, fail.
isNotDoubleQuote(_).
isAt(0'@).
isLt(0'<).
isGt(0'>).
isOpen(0'().
isClose(0')).
isObrace(0'{).
isCbrace(0'}).
isBar(0'|).
isAlpha(C)         :- isLower(C); isUpper(C); isDigit(C); isHyphen(C).
isDigit(C)         :- C >= 0'0, C =< 0'9.
isLower(C)         :- C >= 0'a, C =< 0'z.
isUpper(C)         :- C >= 0'A, C =< 0'Z.
isLetter(C)        :- isLower(C); isUpper(C).



% Read all characters from the input stream
getChars([]) :-
  at_end_of_stream.

getChars([H|T]) :-
  get0(H),
  getChars(T).


% Turn ASCII 10 into line breaks with line counter
ten2nl(Cs0,Cs1) :- ten2nl(1,[10|Cs0],Cs1), !.
ten2nl(_,[],[]).
ten2nl(I0,[H0|T0],[H1|T1]) :-
  (    H0 == 10, H1 = nl(I0), I1 is I0 + 1
  ; \+ H0 == 10, H1 = H0, I1 = I0
  ),
  ten2nl(I1,T0,T1).


% Convert "-" into "_"
hyphen2underscore(0'-,0'_) :- !.
hyphen2underscore(X,X).
