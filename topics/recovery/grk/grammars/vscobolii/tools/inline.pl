:-
   load_files(['../../../lib/io.pl'],
              [if(not_loaded),silent(true)]).


% Skip until the first <HR> is encountered
skiphr(Input) :-
  read(Input,Line),
  ( Line = bar, read(Input,_)
  ; skiphr(Input)
  ).


% Loop for main file
continue(Scratch,Sec,Mods1,Input,Output) :-
  ( at_end_of_stream(Input)
  ; read(Input,Line),
    ( Line = line(List),!,
      format(Output,"~s\n",[List]),!,
      continue(Scratch,Sec,Mods1,Input,Output)
    ;
      Line = bar,!,
      eos(Scratch,Output,Sec,Mods1,Mods1),
      continue(Scratch,nothing,Mods1,Input,Output)
    ; 
      Line = heading(Aname),
      format("Inlining section ""~s"" ...\n",[Aname]),
      continue(Scratch,just(Aname),Mods1,Input,Output) 
    ;
      Line = diagram(NameS),!,
      add(Scratch,Output,NameS,true,Mods1),
      continue(Scratch,Sec,Mods1,Input,Output)
    )
  ).



% Handle modifiers for end-of-section

eos(_,_,nothing,_,_) :- !.

eos(_,_,_,_,[]) :- !.

eos(Scratch,Output,just(Aname),Mods1,[Mod|Mods]) :-
  Mod = add(Com,NameS,eos(Aname)),
  eos(Scratch,Output,just(Aname),Mods1,Mods),
  format(Output,"<PRE>\n",[]),
  Msg = (
    nl(Output),
    format(Output,"@@ Diagram ~s ADDED at the end of this section.\n",[NameS]),
    format(Output,"@@~s\n\n",[Com])
  ),
  add(Scratch,Output,NameS,Msg,Mods1),
  format(Output,"</PRE>\n",[]),
  !.

eos(Scratch,Output,Sec,Mods1,[_|Mods]) :-
  eos(Scratch,Output,Sec,Mods1,Mods).


% Add a diagram; handle everything before and after as well
add(Scratch,Output,NameS,Msg,Mods1) :-
  (
    append(_,[del(Com,NameS)|_],Mods1),
    format(Output,"@@ Diagram ~s DELETED.\n",[NameS]),
    format(Output,"@@~s\n\n",[Com]),
    before(Scratch,Output,NameS,Mods1,Mods1),
    after(Scratch,Output,NameS,Mods1,Mods1)
  ;
    name(Name,NameS),
    format("Inlining diagram ~s ...\n",[NameS]),  
    concat_atom([Scratch,'/',Name,'.pp'],File),
    Msg,
    before(Scratch,Output,NameS,Mods1,Mods1),
    open(File,read,Diagram),
    diagram(Diagram,Output),
    close(Diagram),
    nl(Output),
    after(Scratch,Output,NameS,Mods1,Mods1)
  ).


% Implementation of modifier for after a diagram

after(_,_,_,_,[]) :- !.

after(Scratch,Output,AfterS,Mods1,[Mod|Mods]) :-
  ( Mod = extract(Com,NameS,AfterS),
    Msg = ( nl(Output),
            format(Output,"@@ Diagram ~s EXTRACTED from diagram ~s.\n",[NameS,AfterS]),
            format(Output,"@@~s\n\n",[Com])
        )
  ; Mod = add(Com,NameS,after(AfterS)),
    Msg = ( nl(Output),
            format(Output,"@@ Diagram ~s ADDED after diagram ~s.\n",[NameS,AfterS]),
            format(Output,"@@~s\n\n",[Com])
        )
  ),
  !,
  after(Scratch,Output,AfterS,Mods1,Mods),
  add(Scratch,Output,NameS,Msg,Mods1),
  !.

after(Scratch,Output,NameS,Mods1,[_|Mods]) :-
  after(Scratch,Output,NameS,Mods1,Mods).



% Implementation of modifier for before a diagram

before(_,_,_,_,[]) :- !.

before(Scratch,Output,NameS1,Mods1,[Mod|Mods]) :-
  Mod = replace(Com,NameS1),
  !,
  before(Scratch,Output,NameS1,Mods1,Mods),
  format(Output,"@@ Diagram ~s ADAPTED.\n",[NameS1]),
  format(Output,"@@~s\n\n",[Com]),
  !.

before(Scratch,Output,NameS1,Mods1,[Mod|Mods]) :-
  Mod = fold(Com,NameS1,NameS2),
  !,
  before(Scratch,Output,NameS1,Mods1,Mods),
  format(Output,"@@ Diagram ~s FOLDED according to ~s.\n",[NameS1,NameS2]),
  format(Output,"@@~s\n\n",[Com]),
  !.

before(Scratch,Output,NameS1,Mods1,[Mod|Mods]) :-
  Mod = unfold(Com,NameS1,NameS2),
  !,
  before(Scratch,Output,NameS1,Mods1,Mods),
  format(Output,"@@ Diagram ~s UNFOLDED according to ~s.\n",[NameS1,NameS2]),
  format(Output,"@@~s\n\n",[Com]),
  !.

before(Scratch,Output,BeforeS,Mods1,[Mod|Mods]) :-
  Mod = add(Com,NameS,before(BeforeS)),
  !,
  before(Scratch,Output,BeforeS,Mods1,Mods),
  Msg = ( nl(Output),
          format(Output,"@@ Diagram ~s ADDED before diagram ~s.\n",[NameS,BeforeS]),
          format(Output,"@@~s\n\n",[Com])
        ),
  add(Scratch,Output,NameS,Msg,Mods1),
  !.

before(Scratch,Output,NameS,Mods1,[_|Mods]) :-
  before(Scratch,Output,NameS,Mods1,Mods).



% Loop for diagram file
diagram(Input,Output) :-
  ( at_end_of_stream(Input)
  ; read_line_to_codes(Input,Line),
    format(Output,"~s\n",[Line]),
    diagram(Input,Output)
  ).



:- 
  unix(argv(Argv)),
  append(_,[--,SkeletonName,ModsName,ResultName,Scratch,Arg3],Argv),
  open(SkeletonName,read,Input),
  open(ModsName,read,ModsFile),
  read(ModsFile,Mods),
  close(ModsFile),
  open(ResultName,write,Output),
  skiphr(Input),
  read_file_to_codes(Arg3,Disclaimer,[]),
  format(Output,"~s",[Disclaimer]),
  continue(Scratch,nothing,Mods,Input,Output),
  close(Input),
  close(Output),
  halt.
