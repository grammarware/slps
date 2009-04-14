:-
   load_files([phase1,phase2,
               '../../lib/io.pl',
               '../../tools/fst/normalise.pl'
              ],
              [silent(true)]),

   unix(argv(Argv)), append(_,[--,In,Out],Argv),
   see(In), read(dia(Cs,Ast)),
   ( nl2list(Ast,Asts), Mode = concat
   ; p2list(Ast,Asts), Mode = p
   ),
   !,
   format('Pretty-printing ~a ...\n',[In]),
   ( diagram(Mode,Cs,Asts,Ts),
     tell(Out), ts2cs(Ts), told,
     halt
   ;
     halt(1)
   ).
