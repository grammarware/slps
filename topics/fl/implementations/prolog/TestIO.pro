:- ['Parser.pro'].
:- ['PrettyPrinter.pro'].

main(File1,File2)
 :-
    parseFile(File1,program,Fs),
    ppToFile(File2,Fs).
