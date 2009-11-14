:- ['Evaluation.pro'].

% See slide 157
:- substitute(var(z),x,var(x),T),                     write(T), nl. % var(z)
:- substitute(var(z),x,lam(x,app(var(x),var(x))),T),  write(T), nl. % lam(x,app(var(x),var(x))
:- substitute(var(z),x,lam(y,app(var(y),var(x))),T),  write(T), nl. % lam(y,app(var(y),var(z))
% :- substitute(var(z),x,lam(z,app(var(x),var(z))),T),  write(T), nl. % lam(a,app(var(z),var(a))

:- halt.
