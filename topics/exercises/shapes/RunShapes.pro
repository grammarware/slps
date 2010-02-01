:- ['Shapes.pro'].

main :-
	shape(1,2,S1),
	circle(1,2,3,C1),
	setradius(5,C1,C2),
	rectangle(1,2,3,4,R1),
	setheight(10,R1,R2),
	setwidth(100,R2,R3),
	boldcircle(1,2,3,10,B1),
	setboldness(2,B1,B2),
	movebyall(5,10,[S1,C2,R3,B2],TS2),
	drawall(TS2),
	nl.
