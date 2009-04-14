% Read-file Loop
continue(Input,Output,Scratch) :-
  ( at_end_of_stream(Input)
  ; read_line_to_codes(Input,Line),
    !,
    diagrams(Line,Input,Output,Scratch)
  ).


% Look for <HR> as the end-of-section marker
diagrams(Line0,Input,Output,Scratch) :- 
  ( concat([X1,"<hr>",X2],Line0)
  ; concat([X1,"<HR>",X2],Line0)
  ),
  \+ (member(C1,X1), \+ isSpace(C1)),
  \+ (member(C2,X2), \+ isSpace(C2)),
  format(Output,"bar.\n",[]),
  format(Output,"line(~w).\n",[Line0]),
  !,
  continue(Input,Output,Scratch).


% Process a section
diagrams(Line0,Input,Output,Scratch) :- 
  concat([_,"<A NAME=",Aname,"><H",[C],">",_,"</H",[C],"></A>",_],Line0),
  isDigit(C),
  format(Output,"heading(~w).\n",[Aname]),
  format(Output,"line(~w).\n",[Line0]),
  format("Extracting section ""~s"" ...\n",[Aname]),
  !,
  continue(Input,Output,Scratch).


% Process a diagram
diagrams(Line0,Input,Output,Scratch) :-
  length(X,3),
  concat([X," ___ <B>",NameS,"</B> _",_],Line0),
  retract(header(NameS)),
  name(Name,NameS),
  concat_atom([Scratch,'/',Name,'.dia'],File),
  format("Extracting diagram ~s ...\n",[NameS]),  
  format(Output,"diagram(~w).\n",[NameS]),
  open(File,write,Diagram),
  diagram(Line0,Input,Diagram),
  close(Diagram),
  continue(Input,Output,Scratch).


% Skip a line of text
diagrams(Line,Input,Output,Scratch) :-
  format(Output,"line(~w).\n",[Line]),
  continue(Input,Output,Scratch).


% Copy a diagram
diagram(Line0,Input,Diagram) :-
  append([_,_],Rest0,Line0),
  append("  ",Rest0,Line1),
  html2tt(Line1,Line2),
  format(Diagram,"~s\n",[Line2]),
  ( append(Spaces,Rest1,Line2),
    \+ (Spaces == []), 
    \+ ((member(X,Spaces), \+ (X==0' ))),
    append("|___",_,Rest1)
  ; read_line_to_codes(Input,Line3),
    diagram(Line3,Input,Diagram)
  ).


:- dynamic header/1.

:-
   load_files([library(readutil),
               '../../../lib/io.pl',
               '../../../lib/traversal.pl',
               '../../../lib/html.pl',
               'headers.pl'
              ],[silent(true)]),

   unix(argv(Argv)), append(_,[--,Edit,Skeleton,Scratch],Argv),
   open(Edit,read,Input),
   open(Skeleton,write,Output),
   continue(Input,Output,Scratch),
   close(Input),
   close(Output),
   !,
   \+ retract(header(_)),
   halt.

