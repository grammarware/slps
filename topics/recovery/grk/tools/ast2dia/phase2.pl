% Organize loop over relevant coordinates
ts2cs(Ts) :-
  ymax(Ts,0,Ymax),
  ymin(Ts,0,Ymin),
  xmax(Ts,0,Xmax),
  ts2cs(Ts,0,Ymax,Xmax,Ymin,Ymax).

% Termination condition
ts2cs(_,_,Y1,Xmax,Ymin,_) :-
  Y1 =< Ymin,
  Len is Xmax - 3,
  times(Len,write(' ')),
  write('|'),
  nl,
  write('   |'),
  times(Len,write('_')),
  write('|').

% Line break condition
ts2cs(Ts,X1,Y1,Xmax,Ymin,Ymax) :-
  X1 > Xmax,
  Y2 is Y1 - 1,
  ( Y1 = Ymax
  ; \+ Y1 = Ymin,
    write('|')
  ),
  nl,
  write('   |'),
  ts2cs(Ts,4,Y2,Xmax,Ymin,Ymax).

% Horizontal output generation
ts2cs(Ts1,X1,Y1,Xmax,Ymin,Ymax) :-
  select((T,X1,X2,Y2,Y1),Ts1,Ts2),
  Y2 is Y1 - 1,
  ( T = start, write('>>')
  ; T = finish, write('><')
  ; T = afirst,
        write('    '),
        Len1 is Xmax - X1 - 3,
        times(Len1,write('_'))
  ; T = first(Cs),
        format('    ___ <A NAME=~s>~s</A> ',[Cs,Cs]),
        length(Cs,Len0),
        Len1 is Xmax - Len0 - 8,
        times(Len1,write('_'))
  ; T = lcontinue, write('>_')
  ; T = rcontinue, write('_>')
  ; T = pcontinue, write('||')
  ; T = footnote(Cs), write('@'), name(N,Cs), write(N)
  ; T = obrace, write('{')
  ; T = cbrace, write('}')
  ; T = hline, O is X2 - X1, times(O,write('_'))
  ; T = arrow, O is X2 - X1 - 1, write('<'), times(O,write('_'))
  ; T = terminal(Cs), name(N,Cs), write(N)
  ; T = nonterminal(Cs0),
        ( append(Cs1,[0'-,D],Cs0), isDigit(D)
        ; append(Cs1,[0'-,D1,D2],Cs0), isDigit(D1), isDigit(D2)
        ; Cs1 = Cs0
        ),
        format("<A HREF=#~s>~s</A>",[Cs1,Cs0])
  ),
  ts2cs(Ts2,X2,Y1,Xmax,Ymin,Ymax).

% Vertical vline generation
ts2cs(Ts,X1,Y0,Xmax,Ymin,Ymax) :-
  member((vline,X1,X2,Y2,Y1),Ts),
  X2 is X1 + 1,
  Y0 =< Y1,
  Y0 >  Y2,
  write('|'),
  ts2cs(Ts,X2,Y0,Xmax,Ymin,Ymax).  

% Emit a space
ts2cs(Ts,X1,Y1,Xmax,Ymin,Ymax) :-
  ( Y1 = Ymax
  ; \+ Y1 = Ymin, write(' ')
  ),
  X2 is X1 + 1,
  ts2cs(Ts,X2,Y1,Xmax,Ymin,Ymax).


% Repeated execution
times(0,_).
times(N1,G) :- G, N2 is N1 - 1, times(N2,G).
