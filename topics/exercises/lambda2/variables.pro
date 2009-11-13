:- ['syntax.pro'].

%% see slide 153

free(x,[x]).
free(y,[y]).
free(z,[z]).
free(f,[f]).
free(g,[g]).
free(h,[h]).

free(apply(M,N),FV) :-
 free(M,FV1),
 free(N,FV2),
 append(FV1,FV2,FV).

free(lambda(X,M),FV) :-
 free(M,FV1),
 (
  append(L1,[X|L2],FV1),
  append(L1,L2,FV)
 ;
  FV = FV1
 ).

