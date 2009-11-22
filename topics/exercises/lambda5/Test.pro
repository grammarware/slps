:- ['Typing.pro'].

%
% Testing typing rules; see slide 165
%
:- 
 hastype([],app(lam(f,maps(bool,bool),app(var(f),false)),lam(g,bool,var(g))),T),
 write(T), nl.
