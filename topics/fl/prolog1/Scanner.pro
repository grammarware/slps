:- ['ParserLib.pro'].

% Withspace excluding eoln

spaces --> munch(isMySpace,_).

isMySpace(X) :- isSpace(X), \+ isEoln(X).


% Keywords of FL

isReserved(if).
isReserved(then).
isReserved(else).

reserved(S) --> spaces, string(S), ( follows(isSpace) ; eof ).


% Special characters

special(S) --> spaces, string(S).


% Newline token

newline --> spaces, satisfy(isEoln,_).


% Integers with optional negative sign

int(I) --> 
    spaces,
    option(0'+,char(0'-),Sign),
    munch1(isDigit,Digits),
    { name(I,[Sign|Digits]) } .


% Names that are not keywords

name(N) --> 
    spaces,
    munch1(isLower,S),
    { name(N,S), \+ isReserved(N) }.
