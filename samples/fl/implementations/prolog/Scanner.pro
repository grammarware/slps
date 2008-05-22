% Withspace excluding eoln

spaces --> munch(isMySpace,_).

isMySpace(X) :- isSpace(X), \+ isEoln(X).


% Keywords of FL

isKeyword(if).
isKeyword(then).
isKeyword(else).

keyword(S) --> spaces, string(S), ( follows(isWhitespace) ; eof ).


-- Special characters

special(S) --> spaces, string(S).



% Token classes with withspace handling


int(I) --> 
    spaces,
    option(0'+,char(0'-),Sign),
    munch1(isDigit,Digits),
    { name(I,[Sign|Digits]) } .

name(N) --> spaces, munch1(isLower,S), { name(N,S), \+ isKeyword(N) }.

eoln --> spaces, satisfy(isEoln,_).
