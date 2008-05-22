:- ['Library.pro','Scanner.pro'].
% :- set_prolog_flag(double_quotes,atom).
:- set_prolog_flag(double_quotes,codes).
% :- set_prolog_flag(double_quotes,string.


% Programs as lists of function definitions

program(Fs) --> many1(function,Fs).


% Function definitions

function((N,Ns,E)) -->
       name(N),
       many(name,Ns),
       special("="),
       expr(E),
       many1(eoln).


% Top-level layer of expression forms

expr(E) --> lassoc(ops,atom,binary,E).

expr(apply(N,Es)) -->
       name(N),
       many(atom,Es).

expr(ifThenElse(E1,E2,E3)) -->
       keyword("if"),
       expr(E1),
       keyword("then"),
       expr(E2),
       keyword("else"),
       expr(E3).


% Final layer of expression forms

atom(literal(I)) --> int(I).
atom(argument(N)) --> name(N).
atom(E) --> special("("), expr(E), special(")").


% Operation symbols

ops(equal) --> special("==").
ops(plus) --> special("+").
ops(minus) --> special("-").
