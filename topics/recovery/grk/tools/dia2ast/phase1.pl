/* Recognizing token streams */

cs2ts(QTs,Cs) :- qtokens(QTs,Cs,([],_,_)), !.

qtokens(QTs)      --> layout, qtokens(QTs).
qtokens([QT|QTs]) --> qualify(token,QT), qtokens(QTs).
qtokens([])       --> [].


/* Recognizing tokens */

token(start,Y,Y)           --> seq(char(isGt),char(isGt),_).
token(finish,Y,Y)          --> seq(char(isGt),char(isLt),_).
token(continue,Y,Y)        --> char(isGt,_).
token(obrace,Y,Y)          --> char(isObrace,_).
token(cbrace,Y,Y)          --> char(isCbrace,_).
token(footnote(Cs),Y,Y)    --> char(isAt,_),char(isDigit,Cs).
token(terminal(Cs),Y,Y)    --> seq(char(isUpper),plus(char(isAlpha)),Cs).
token(terminal(Cs),Y,Y)    --> plus(char(isDigit),Cs).
token(terminal(Cs),Y,Y)    --> doublequoted(Cs).
token(nonterminal(Cs),Y,Y) --> seq(char(isLower),plus(char(isAlpha)),Cs).
token(hline,Y,Y)           --> plus(char(isUnderscore),_).
token(arrow,Y,Y)           --> seq(char(isLt),plus(char(isUnderscore)),_).
token(vline,_,undefined)   --> char(isBar,_).
token(terminal(Cs),Y,Y)    --> plus(char(isSpecial),Cs).


% "Special" characters
isSpecial(C)       :- C =\= 0' , C =\= 0'_.


/* Recognizing layout */

layout --> eoln. 
layout --> plus(char(isSpace),_).


/* Double-quoted terminals */

doublequoted(Cs) -->
  char(isDoubleQuote,_),
  plus(char(isNotDoubleQuote),Cs),
  char(isDoubleQuote,_).



/* Regular operators */

char(P,[C],([C|Cs],X0,Y),(Cs,X1,Y))
 :-
    call(P,C),
    X1 is X0 + 1.

eoln(([C|Cs],_,Y0),(Cs,0,Y1))
 :-
    ( C == 10; C == 13 ),
    Y1 is Y0 - 1.

seq(P1,P2,Cs0,A0,A2) 
 :-
    call(P1,Cs1,A0,A1),
    call(P2,Cs2,A1,A2),
    append(Cs1,Cs2,Cs0).

plus(P,Cs,A0,A1)
 :-
    seq(P,star(P),Cs,A0,A1).

star(P,Cs,A0,A1)
 :-
    opt(plus(P),Cs,A0,A1).

opt(P,Cs,A0,A1)
 :-
      call(P,Cs,A0,A1)
    ; A1 = A0, Cs = [].


/* Capturing position information */

qualify(P,(T,X0,X1,Y0,Y1,Ycenter),A0,A1)
 :-
    A0 = (_,X0,Y1),
    A1 = (_,X1,Y1),
    Y0 is Y1 - 1,
    call(P,T,Y0,Ycenter,A0,A1).

