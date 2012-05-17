% see slides #109, #113
term(true).
term(false).
term(zero).
term(succ(T)) :- term(T).
term(pred(T)) :- term(T).
term(iszero(T)) :- term(T).
term(if(T1,T2,T3)) :- term(T1), term(T2), term(T3).

