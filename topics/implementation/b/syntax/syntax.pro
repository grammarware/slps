% see slide #109
term(true).
term(false).
term(if(T1,T2,T3)) :- term(T1), term(T2), term(T3).

