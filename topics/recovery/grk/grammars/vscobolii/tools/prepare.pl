% Loop over the document line-per-line
loop(Input,_,_,html) :- at_end_of_stream(Input), !.
loop(Input,Output,Pos0,Loc0) :-
  read_line_to_codes(Input,LineIn),
  replace(Input,Output,LineIn,Y,Pos0,Loc0,Loc1),
  (
    Y = just(LineOut),
    format(Output,"~s\n",[LineOut])
  ;
    Y = nothing
  ),
  (
    Loc1 =.. [F,Push,Pop],
    member(F,[dia,frag]),
    !,
    Pop == [],
    Loc2 =.. [F,[],Push]
  ;
    Loc2 = Loc1
  ),
  Pos1 is Pos0 + 1,
  !,
  loop(Input,Output,Pos1,Loc2).



% Skip <base href=...>
replace(_,_,_,nothing,4,Loc,Loc) :-
  !,
  Loc == html.


% Skip first header "IBM BookManager Print Preview"
replace(_,_,L0,just(L0),8,Loc,Loc) :-
  !,
  Loc == html.


% Skip non-diagrams
replace(_,_,L0,just(L0),X,Loc,Loc) :- 
  Loc == html,
  not_a_diagram(X),
  !.


% Place disclaimer everywhere
replace(_,O,L0,nothing,_,Loc,Loc) :- 
  Loc == html,
  concat([X1,"<hr>",X2],L0),
  \+ (member(C1,X1), \+ isSpace(C1)),
  \+ (member(C2,X2), \+ isSpace(C2)),
  format(O,"~s\n",[L0]),
  format(O,"<I><small><B>Disclaimer:</B> This is not an official IBM document.
            Make sure that you understand the full <A HREF=#disclaimer>disclaimer text</A>
            as a prerequisite to using the present document.</small></I>\n",[]),
  !.


% Handle all <a name=...>
replace(I,O,L0,Y,Pos,Loc,Loc) :-
  Loc == html,
  concat([X1,"<a",Q,"name=""",F0,""">",X2,"</a>",X3],L0),
  \+ member(0'>,Q),
  \+ member(0'>,F0),
  Q = [32|_],
  reverse(Q,[32|_]),
  concat([X1,X2,X3],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc,Loc).


% Handle all <a href=...>
replace(I,O,L0,Y,Pos,Loc,Loc) :-
  Loc == html,
  concat([X1,"<a",Q,"href=""",F0,""">",X2,"</a>",X3],L0),
  \+ member(0'>,Q),
  \+ member(0'>,F0),
  Q = [32|_],
  reverse(Q,[32|_]),
  ( 
    ahref(F0,F1),
    concat([X1,"<A HREF=#",F1,">",X2,"</A>",X3],L1)
  ;
    concat([X1,X2,X3],L1)
  ),
  !,
  replace(I,O,L1,Y,Pos,Loc,Loc).


% Handle all <img src=...>
replace(I,O,L0,Y,Pos,Loc,Loc) :-
  Loc == html,
  concat([X1,"<img",Q,"src=""",F,"""",X2],L0),
  \+ member(0'>,Q),
  \+ member(0'>,F),
  \+ append("figures/p",_,F),
  Q = [32|_],
  reverse(Q,[32|_]),
  append(_,[C,0'.|_],F),
  concat([X1,"<IMG",Q,"SRC=""figures/",[0'p,C],".gif""",X2],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc,Loc).  


% Handle all headers
replace(_,_,L0,just(L1),_,Loc,Loc) :-
  Loc == html,
  ( H = 0'H; H = 0'h ),
  append(X1,[0'<,H,D,0'>|Y],L0),
  append(F,[0'<,0'/,H,D,0'>|X2],Y),
  !,
  aname(F,N),
  format("Preparing section ""~s"" ...\n",[N]),
  concat([X1,"<A NAME=",N,">",[0'<,0'H,D,0'>],F,[0'<,0'/,0'H,D,0'>],"</A>",X2],L1).


% Handle 1st line of diagrams
replace(_,_,L0,just(L1),Pos,Loc,dia([],[])) :-
  Loc == html,
  X1 = [_,_,_],
  (
    concat([X1," ___ <B>Format",X2," _",X3,X4],L0)
  ;
    % special case for ALTER diagram
    Pos = 14579,
    concat([X1," ____________",X3,X4],L0), X2 = []
  ),
  foreach(isUnderscore,X3),
  foreach(isSpace,X4),
  !,
  retract(header(D)),
  length(D,Len),
  html2tt(X2,X2tt),
  length(X2tt,Len2),
  length(X3,Len3),
  Len0 is Len3 + 7 + Len2 - Len,
  length(R,Len0),
  foreach(isUnderscore,R),
  concat([X1," ___ <B>",D,"</B> ",R],L1).


% Handle end of diagram
replace(_,_,L,just(L),_,Loc,html) :-
  member(Loc,[dia(Push,Pop),frag(Push,Pop)]),
  [_,_,_,0'|,0'_|_] = L,
  !,
  Push == [],
  Pop  == [].


% Handle end of notes
replace(_,_,L,just(L),_,note,html) :-
  [_,_,_,0'|,0'_|_] = L,
  !.


% Turn "|" into ">>" for fragments
replace(I,O,L0,Y,Pos,Loc0,Loc1) :-
  Loc0 = frag(_,_),
  length(X1,3),
  concat([X1,"| |__",X2],L0),
  concat([X1,"| >>_",X2],L1),
  replace(I,O,L1,Y,Pos,Loc0,Loc1),
  !.


% Turn "|" into "><" for fragments
replace(I,O,L0,Y,Pos,Loc0,Loc1) :-
  Loc0 = frag(_,_),
  concat([X1,"__| |",X2],L0),
  foreach(isSpace,X2),
  concat([X1,"_>< |",X2],L1),
  replace(I,O,L1,Y,Pos,Loc0,Loc1),
  !.


% Pointwise replacements
replace(I,O,L0,Y,Pos,Loc0,Loc1) :-
  member(Loc0,[dia(_,_),frag(_,_)]),
  retract(replace(Pos,Old,New)),
  !,
  concat([X1,Old,X2],L0),
  concat([X1,New,X2],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc0,Loc1).


% Space-to-underscore conversion in between terminals
replace(I,O,L0,Y,Pos,Loc0,Loc1) :-
  member(Loc0,[dia(_,_),frag(_,_)]),
  concat([X1,[X2],"</kbd> <kbd>",[X3],X4],L0),
  ( isLetter(X2)
  ; isLetter(X3)
  ),
  concat([X1,[X2],"</kbd>_<kbd>",[X3],X4],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc0,Loc1).


% Space-to-underscore conversion
replace(I,O,L0,Y,Pos,Loc0,Loc1) :-
  member(Loc0,[dia(_,_),frag(_,_)]),
  concat([X1,"_ <kbd>",[X2],X3],L0),
  isLetter(X2),
  concat([X1,"__<kbd>",[X2],X3],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc0,Loc1).


% Handle references to fragments
replace(I,O,L0,Y,Pos,Loc0,Loc1) :- 
  member(Loc0,[dia(_,_),frag(_,_)]),
  retract(fragref(Pos,Old,New)),
  !,
  length(Old,OL),
  length(New,NL),
  UL is OL + 4 - NL,
  UL >= 0,
  length(Us,UL),
  foreach(isUnderscore,Us), 
  concat([X1,"| ",Old," |",X2],L0),
  concat([X1,"<var>",New,"</var>",Us,X2],L1),
  !,
  replace(I,O,L1,Y,Pos,Loc0,Loc1).


% Push footnote marks as they appear in the air
replace(I,O,L0,Y,Pos,Loc0,Loc2) :- 
  Loc0 =.. [F,Push,Pop],
  member(F,[dia,frag]),
  concat([X1," (",[C],")",X2],L0),
  isDigit(C),
  !,
  html2tt(X1,X1tt),
  length(X1tt,Len),
  concat([X1,"    ",X2],L1),
  Loc1 =.. [F,[(C,Len)|Push],Pop],
  replace(I,O,L1,Y,Pos,Loc1,Loc2).


% Pop footnote marks as they suite into lists
replace(I,O,L0,Y,Pos,Loc0,Loc2) :- 
  Loc0 =.. [F,Push,[(C,Len)|Pop]],
  member(F,[dia,frag]),
  !,
  concat([X1,"____",X2],L0),
  html2tt(X1,X1tt),
  length(X1tt,Len),
  Loc1 =.. [F,Push,Pop],
  concat([X1,"_@",[C],"_",X2],L1),
  replace(I,O,L1,Y,Pos,Loc1,Loc2).


% Handle notes
replace(_,O,L,nothing,_,Loc,note) :-
  member(Loc,[dia(Push,Pop),frag(Push,Pop)]),
  ( append("   | <B>Note:</B>",X,L), N0 = 0
  ; append(" | | <B>Note:</B>",X,L), N0 = 0
  ; append("   | <B>Notes:</B>",X,L), N0 = 1
  ; append(" | | <B>Notes:</B>",X,L), N0 = 1
  ),
  append(X1,[0'||X2],X),
  foreach(isSpace,X1),
  foreach(isSpace,X2),
  !,
  Push == [],
  Pop  == [],
  length(X1,N1),
  N2 is N0 + N1 + 6,
  write(O,'   |'),
  times(N2,write(O,'_')),
  write(O,'|'),
  nl(O),
  write(O,'    '),
  times(N2,write(O,'_')),
  nl(O),
  write(O,'   |'),
  times(N2,write(O,' ')),
  write(O,'|'),
  nl(O),
  format(O,"~s\n",[L]),
  !.


% Handle syntax fragments
replace(_,O,L0,Y,_,Loc0,Loc1) :-
  Loc0 =.. [F,Push,Pop],
  member(F,[dia,frag]),
  concat([_,":</B>",_,"|",S],L0),
  foreach(isSpace,S),
  retract(header(D)),  
  !,
  Loc1 =.. [frag,Push,Pop],
  length(D,Len),
  length(S,Len1),
  html2tt(L0,L0tt),
  length(L0tt,Len2),
  Len3 is Len2 - Len1 - 5,
  write(O,'   |'),
  times(Len3,write(O,'_')),
  write(O,'|'),
  nl(O),
  Len0 is Len3 - Len - 5,
  length(R,Len0),
  foreach(isUnderscore,R),
  concat(["    ___ <B>",D,"</B> ",R],L1),
  format(O,"~s\n",[L1]),
  write(O,'   |'),
  times(Len3,write(O,' ')),
  write(O,'|'),
  nl(O),
  Y = nothing.


% Fixpoint reached
replace(_,_,L,just(L),_,Loc,Loc).



% Map a-refs to local paragaphs
ahref(Cs,Cs) :- special(Cs).
ahref(Cs0,[0'F,0'.|Cs2]) :- append("FRONT_",Cs1,Cs0), ahref1(Cs1,Cs2).
ahref(Cs0,[0'A,0'.|Cs2]) :- append("APPENDIX",Cs1,Cs0), ahref1(Cs1,Cs2).
ahref(Cs0,[0'B,0'.|Cs2]) :- append("BACK_",Cs1,Cs0), ahref1(Cs1,Cs2).
ahref(Cs0,Cs1) :- ahref1(Cs0,Cs1).
ahref1([C|Cs0],[C|Cs1]) :- C >= 0'0, C =< 0'9, ahref2(Cs0,Cs1).
ahref2([C|Cs0],[C|Cs1]) :- C >= 0'0, C =< 0'9, ahref2(Cs0,Cs1).
ahref2([],[]).
ahref2([0'%,0'2,0'e|Cs0],[0'.|Cs1]) :- ahref1(Cs0,Cs1).
ahref2([0'#|_],[]).
ahref2([0'?|_],[]).



% Special sections without numbering
special("COVER").
special("ABSTRACT").
special("NOTICES").
special("EDITION").
special("CONTENTS").
special("FIGURES").
special("INDEX").
special("COMMENTS").



% Map <H1>, ... to a-names
aname([32|F],N) :- aname1(F,N).
aname([0'|,32|F],N) :- aname1(F,N).
aname([0'x,32|F],N) :- aname1(F,N).
aname(_,_) :- trace,halt(1).

aname1(F0,N) :-
  append(F1,[32|_],F0),
  \+ member(32,F1),
  !,
  aname2(F1,N).

aname2(Cs,Cs) :- special(Cs).
aname2(Cs0,[0'F,0'.|Cs1]) :- append("FRONT_",Cs1,Cs0), aname3(Cs1).
aname2(Cs0,[0'A,0'.|Cs1]) :- append("APPENDIX",Cs1,Cs0), aname3(Cs1).
aname2(Cs0,[0'B,0'.|Cs1]) :- append("BACK_",Cs1,Cs0), aname3(Cs1).
aname2(Cs,Cs) :- aname3(Cs).
aname3([C|Cs]) :- C >= 0'0, C =< 0'9, aname4(Cs).
aname4([C|Cs]) :- C >= 0'0, C =< 0'9, aname4(Cs).
aname4([]).
aname4([0'.|Cs]) :- aname3(Cs).



:- dynamic header/1.
:- dynamic fragref/3.
:- dynamic replace/3.

:-
   load_files([library(readutil),
               '../../../lib/io.pl',
               '../../../lib/traversal.pl',
               '../../../lib/html.pl',
               'headers.pl',
               'fragrefs.pl',
               'skip.pl',
               'replace.pl'
              ],[silent(true)]),
   unix(argv(Argv)), append(_,[--,Arg1,Arg2,Arg3],Argv),
   open(Arg1,read,Input,[]),
   open(Arg2,write,Output,[]),
   read_file_to_codes(Arg3,Disclaimer,[]),
   format(Output,"~s",[Disclaimer]),
   loop(Input,Output,1,html),
   close(Output),
   close(Input),
   !,
   \+ retract(header(_)),
   \+ retract(fragref(_,_,_)),
   \+ retract(replace(_,_,_)),
   halt.
