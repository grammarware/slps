
/*

case(Q,UD,T1,T2) 
 :-
    transform(try(xbgf2:case_rule(Q,UD)),T1,T2).

case_rule(Q,UD,g(Rs1,Ps),g(Rs2,Ps)) :- maplist(xbgf1:doCase(Q,UD),Rs1,Rs2).
case_rule(Q,UD,p(As,N1,X),p(As,N2,X)) :- xbgf1:doCase(Q,UD,N1,N2).
case_rule(Q,UD,n(N1),n(N2)) :- xbgf1:doCase(Q,UD,N1,N2).
case_rule(Q,UD,l(L1),l(L2)) :- xbgf1:doCase(Q,UD,L1,L2).
case_rule(Q,UD,s(S1,X),s(S2,X)) :- xbgf1:doCase(Q,UD,S1,S2).

*/
