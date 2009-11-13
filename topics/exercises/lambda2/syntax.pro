% see slides #147

term(x).
term(y).
term(z).
term(f).
term(g).
term(h).

term(apply(M,N)) :- term(M), term(N).
term(lambda(X,M)) :- term(X), term(M).

