:-
   unix(argv(Argv)), append(_,[--,Pl,In],Argv),
   load_files(['../../../lib/io.pl',
               '../../../lib/regexp.pl',
               Pl,
               'lexical.pl'
              ],
              [silent(true)]),
   see(In), getChars(Cs0), seen,
   !,
   ten2nl(Cs0,Cs1),
   !,
   flag(line,0,1),
   whitespaces(Cs1,Cs2),
   ( cobol_source_program(Cs2,Cs3),!
   ; Cs3 = Cs2
   ),
   flag(line,Line,0),
   ( 
     \+ Cs2 == [], Cs3 == [], !,
     format("Parsed ~w lines.\n",[Line]),
     halt
   ; 
     format("Parse error in line ~w.\n",[Line]),
     halt(1)
   ).
