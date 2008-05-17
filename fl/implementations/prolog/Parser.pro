:- ['Library.pro','Scanner.pro'].
% :- set_prolog_flag(double_quotes,atom).
:- set_prolog_flag(double_quotes,codes).
% :- set_prolog_flag(double_quotes,string.


% Parse from a file

parseFile(File,Fs)
 :-
    open(File,read,Stream,[]), 
    read_stream_to_codes(Stream, Contents),
    close(Stream),
    program(Fs,Contents,_).


% Programs as lists of function definitions

program(Fs) --> many1(function,Fs), eof.


% Function definitions

function(((N,Ns),E)) -->
       name(N),
       many(name,Ns),
       special("="),
       expr(E),
       many1(eoln).


% All expression forms

expr(literal(I)) --> int(I).

expr(argument(N)) --> name(N).

expr(binary(O,E1,E2)) --> 
       special("("),
       expr(E1),
       ops(O),
       expr(E2),
       special(")").

expr(ifThenElse(E1,E2,E3)) -->
       keyword("if"),
       expr(E1),
       keyword("then"),
       expr(E2),
       keyword("else"),
       expr(E3).

expr(apply(N,Es)) -->
       special("("),
       name(N),
       many(expr,Es),
       special(")").

ops(equal) --> special("==").
ops(plus) --> special("+").
ops(minus) --> special("-").
