manysteps(V,V) :- value(V).
manysteps(T1,V) :- eval(T1,T2), manysteps(T2,V).

