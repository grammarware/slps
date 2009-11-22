:- ['Evaluation.pro'].

%
% Testing evaluation; see slide 173
%
:- 
 C0 = lam(s,lam(z,var(z))),
 C1 = lam(s,lam(z,app(var(s),var(z)))),
% True = lam(t,lam(f,var(t))),
% False = lam(t,lam(f,var(f))),
% Test = lam(l,lam(m,lam(n,app(app(var(l),var(m)),var(n))))),
% Iszero = lam(n,app(app(var(n),lam(x,False)),True)),
 Succ = lam(n,lam(s,lam(z,app(var(s),app(app(var(n),var(s)),var(z)))))),
 write('C1 is      '), write(C1), nl,
 manysteps(app(Succ,C0),Q),
 write('succ C0 is '),  write(Q), nl.
% λn.λs.λz.s (n s z)

 % Y = lam(f,app(lam(x,app(f,app(x,var(x)))),lam(x,app(f,app(x,var(x)))))),

:- halt.
