substitute(_,_,true,true).
substitute(_,_,false,false).
substitute(_,_,zero,zero).
substitute(N,X,succ(T1),succ(T2)) :- substitute(N,X,T1,T2).
substitute(N,X,pred(T1),pred(T2)) :- substitute(N,X,T1,T2).
substitute(N,X,iszero(T1),iszero(T2)) :- substitute(N,X,T1,T2).
substitute(N,X,if(T1a,T2a,T3a),if(T1b,T2b,T3b)) 
 :- 
    substitute(N,X,T1a,T1b),
    substitute(N,X,T2a,T2b),
    substitute(N,X,T3a,T3b).
