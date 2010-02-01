% Object encoding: [M, T1, T2, ...]
% M - shapedata(X,Y)
% T - circledelta(R)
% T - rectdelta(H,W)
% T - boldcircledelta(W)
% ...

:- discontiguous(drawtail/1).

last([E], E). last([_|T], E) :- last(T, E).

isshape([shapedata(X,Y)|T]) :-
	integer(X),
	integer(Y),
	shapetails(T).
	
shapetails([]).
shapetails([T|TS]) :-
	shapetail(T),
	shapetails(TS).
	
shapetail(circledelta(R)) :-
	integer(R).
shapetail(rectdelta(H,W)) :-
	integer(H),
	integer(W).
shapetail(boldcircledelta(W)) :-
	integer(W).

subclassname([]) :-
	write('abstract shape').
subclassname([circledelta(_)]) :-
	write('circle').
subclassname([boldcircledelta(_)]) :-
	write('bold circle').
subclassname([rectdelta(_,_)]) :-
	write('rectangle').
subclassname([_|SS]) :-
	subclassname(SS).
	
getx([shapedata(X,Y)|T],X) :-
	isshape([shapedata(X,Y)|T]).
gety([shapedata(X,Y)|T],Y) :-
	isshape([shapedata(X,Y)|T]).
setx(NX,[shapedata(X,Y)|T],[shapedata(NX,Y)|T]) :-
	isshape([shapedata(X,Y)|T]),
	isshape([shapedata(NX,Y)|T]).
sety(NY,[shapedata(X,Y)|T],[shapedata(X,NY)|T]) :-
	isshape([shapedata(X,Y)|T]),
	isshape([shapedata(X,NY)|T]).
moveto(NX,NY,S1,S3) :-
	setx(NX,S1,S2),
	sety(NY,S2,S3).
moveby(DX,DY,S1,S3) :-
	getx(S1,X),
	gety(S1,Y),
	NX is X + DX,
	NY is Y + DY,
	setx(NX,S1,S2),
	sety(NY,S2,S3).
% Can be implemented completely differently, this one just shows the (polymorphic) way, not the exact bits:
draw([shapedata(X,Y)|T]) :-
	isshape([shapedata(X,Y)|T]),
	write('Drawing the '),
	subclassname(T),
	write(' at '),
	write(X),
	write(' × '),
	write(Y),
	drawtails(T),
	write('.'),nl.
drawtails([]).
drawtails([T|TS]) :-
	drawtail(T),
	drawtails(TS).

% circles
iscircle(S) :- isshape(S), last(S,circledelta(_)).
getradius([circledelta(R)|_],R).
getradius([_|T],R) :- getradius([T],R).
setradius(NR,[circledelta(_)|T],[circledelta(NR)|T]) :-	integer(NR).
setradius(NR,[S|T1],[S|T2]) :- setradius(NR,T1,T2).
drawtail(circledelta(R)) :- write(' of radius '), write(R).

% rects
isrect(S) :- isshape(S), last(S,rectdelta(_,_)).
getheight([rectdelta(H,_)|_],H).
getheight([_|T],R) :- getheight([T],R).
setheight(NH,[rectdelta(_,W)|T],[rectdelta(NH,W)|T]) :- integer(NH).
setheight(NH,[S|T1],[S|T2]) :- setheight(NH,T1,T2).
getwidth([rectdelta(_,W)|_],W).
getwidth([_|T],R) :- getwidth([T],R).
setwidth(NW,[rectdelta(H,_)|T],[rectdelta(H,NW)|T]) :- integer(NW).
setwidth(NW,[S|T1],[S|T2]) :- setwidth(NW,T1,T2).
drawtail(rectdelta(H,W)) :-
	write(' of size '),
	write(H),
	write(' × '),
	write(W).
	
% boldcircles
isboldcircle(S) :- isshape(S), last(S,boldcircledelta(_)).
getboldness([boldcircledelta(R)|_],R).
getboldness([_|T],R) :- getboldness([T],R).
setboldness(NR,[boldcircledelta(_)|T],[boldcircledelta(NR)|T]) :- integer(NR).
setboldness(NR,[S|T1],[S|T2]) :- setboldness(NR,T1,T2).
drawtail(boldcircledelta(W)) :-
	write(' and width '),
	write(W).

drawall([]).
drawall([S|SS]) :-
	draw(S),
	drawall(SS).
movebyall(_,_,[],[]).
movebyall(DX,DY,[S1|SS1],[S2|SS2]) :-
	moveby(DX,DY,S1,S2),
	movebyall(DX,DY,SS1,SS2).

% constructors
shape(X,Y,[shapedata(X,Y)]) :-
	isshape([shapedata(X,Y)]).
rectangle(X,Y,H,W,S2) :-
	shape(X,Y,S1),
	append(S1,[rectdelta(H,W)],S2),
	isrect(S2).
circle(X,Y,R,S2) :-
	shape(X,Y,S1),
	append(S1,[circledelta(R)],S2),
	iscircle(S2).
boldcircle(X,Y,R,W,S2) :-
	circle(X,Y,R,S1),
	append(S1,[boldcircledelta(W)],S2),
	isboldcircle(S2).
