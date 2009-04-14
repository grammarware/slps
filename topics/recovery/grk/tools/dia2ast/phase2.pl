refactor(Ts0,Ts3)
 :-
    fixpoint(vfuse,Ts0,Ts1),
    fixpoint(vcut,Ts1,Ts2),
    fixpoint(hcut,Ts2,Ts3).


% fixpoint combinator
fixpoint(P,X,Z) :- call(P,X,Y), !, fixpoint(P,Y,Z).
fixpoint(_,X,X) :- !.


% Fuse vertical line segments
vfuse(Ts0,[(vline,X0,X1,Y0,Y2,undefined)|Ts2])
 :-
    select((vline,X0,X1,Y0,Y1,_),Ts0,Ts1),
    select((vline,X0,X1,Y1,Y2,_),Ts1,Ts2).



% Cut vertical line segments
vcut(Ts0,[T2|[T1|Ts1]])
 :-
    select((vline,X0,X1,Y0,Y1,undefined),Ts0,Ts1),
    select((hline,X2,X3,_,_,Ycenter),Ts1,_),
    Ycenter < Y1,
    Ycenter > Y0,
    (X2 == X1; X3 == X0),
    T1 = (vline,X0,X1,Y0,Ycenter,undefined),
    T2 = (vline,X0,X1,Ycenter,Y1,undefined).


% Handle left angle of stacking level
hcut(Ts0,[T2|[T1|Ts1]])
 :-
    select((hline,X0,X1,Y0,Y1,Ycenter0),Ts0,Ts1),
    select((vline,X2,X3,Y2,Y0,undefined),Ts1,_),
    select((hline,X3,_,Y2,_,Y2),Ts1,_),
    X0 < X2, X1 > X2,
    T1 = (hline,X0,X2,Y0,Y1,Ycenter0),
    T2 = (hline,X2,X1,Y0,Y1,Ycenter0).

% Handle right angle of stacking level
hcut(Ts0,[T2|[T1|Ts1]])
 :-
    select((hline,X0,X1,Y0,Y1,Ycenter0),Ts0,Ts1),
    select((vline,X2,X3,Y2,Y0,undefined),Ts1,_),
    select((hline,_,X2,Y2,_,Y2),Ts1,_),
    X0 < X3, X1 > X3,
    T1 = (hline,X0,X3,Y0,Y1,Ycenter0),
    T2 = (hline,X3,X1,Y0,Y1,Ycenter0).

% Handle arrow
hcut(Ts0,[T2|[T1|Ts1]])
 :-
    select((hline,X0,X1,Y0,Y1,Ycenter0),Ts0,Ts1),
    select((arrow,X2,X3,Y2,_,Y2),Ts1,Ts2),
    Y2 >= Y1, X0 < X2, X1 > X2,
    select((vline,X3,_,Y0,Y2,_),Ts2,_),
    T1 = (hline,X0,X2,Y0,Y1,Ycenter0),
    T2 = (hline,X2,X1,Y0,Y1,Ycenter0).
