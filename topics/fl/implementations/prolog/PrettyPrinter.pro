% Pretty-print to file

ppToFile(File,Fs)
 :-
    prettyPrint(Fs,Contents),
    open(File,write,Stream,[]), 
    write(Stream, Contents),
    close(Stream).


prettyPrint([],"").
prettyPrint([H|T],S)
 :-
    prettyPrint(H,S1),
    prettyPrint(T,S2),
    format(string(S),"~s~n~s", [S1,S2,S]).

prettyPrint((N,Ns,E),S2)
 :-
    ppFormals("",Ns,Ss),
    prettyPrint(E,S1),
    format(string(S2),"~w~s = ~s", [N,Ss,S1]).

prettyPrint(literal(I),S)
 :- 
    string_to_atom(S,I).

prettyPrint(argument(A),S)
 :- 
    string_to_atom(S,A).

prettyPrint(binary(O,X,Y),S)
 :- 
    prettyPrint(X,S1),
    prettyPrint(Y,S2),
    ( O == equal, SO = "=="
    ; O == plus, SO = " + "
    ; O == minus, SO = " - "
    ),
    format(string(S),"(~s~s~s)", [S1,SO,S2]).

prettyPrint(ifThenElse(X,Y,Z),S)
 :- 
    prettyPrint(X,S1),
    prettyPrint(Y,S2),
    prettyPrint(Z,S3),
    format(string(S),"if ~s then ~s else ~s", [S1,S2,S3]).

prettyPrint(apply(N,Es),S)
 :- 
    ppActuals("",Es,Ss),
    format(string(S),"(~w~s)", [N,Ss]).

ppFormals(S,[],S).
ppFormals(S1,[N|Ns],S3) 
 :- 
    format(string(S2),"~s ~w", [S1,N]),
    ppFormals(S2,Ns,S3).

ppActuals(S,[],S).
ppActuals(S1,[E|Es],S4) 
 :- 
    prettyPrint(E,S2),
    format(string(S3),"~s ~s", [S1,S2]),
    ppActuals(S3,Es,S4).
