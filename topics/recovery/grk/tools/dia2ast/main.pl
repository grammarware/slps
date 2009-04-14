:-
   load_files([phase1,phase2,phase3,
               '../../lib/io.pl',
               '../../lib/traversal.pl',
               '../../tools/fst/normalise.pl'
              ],
              [silent(true)]),

   unix(argv(Argv)), append(_,[--,In,Out],Argv),
   see(In), getChars(Cs), seen,
   format('Parsing ~a ...\n',[In]),
   cs2ts(Ts0,(Cs,0,0)),
   refactor(Ts0,Ts1),
   (
     diagram(NameS,Asts0,Ts1,[]),
     list2nl(Asts0,Ast1),
     normalise(Ast1,Ast2),
     tell(Out), format("~w.\n",[dia(NameS,Ast2)]), told,
     halt
   ;
     halt(1)
   ).

