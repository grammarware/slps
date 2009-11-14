:- ['Evaluation.pro'].

%
% Testing subsititution; see slide 157
%
:- substitute(var(z),x,var(x),T),                     write(T), nl. % var(z)
:- substitute(var(z),x,lam(x,app(var(x),var(x))),T),  write(T), nl. % lam(x,app(var(x),var(x))
:- substitute(var(z),x,lam(y,app(var(y),var(x))),T),  write(T), nl. % lam(y,app(var(y),var(z))

/*

The following test case (taken from a slide of the lecture) will not pass.

:- substitute(var(z),x,lam(z,app(var(x),var(z))),T),  write(T), nl. % lam(a,app(var(z),var(a))

The trouble is about variable z occurring once freely and once bound.
The implemented substitution must be enhanced to perform alpha conversion.

*/

%
% Testing evaluation; see slide 165
%
:- 
True = lam(t,lam(f,var(t))),
% False = lam(t,lam(f,var(f))),
Test = lam(l,lam(m,lam(n,app(app(var(l),var(m)),var(n))))),
manysteps(app(app(app(Test,True),var(v)),var(w)),Q),
write(Q), nl.

:- halt.
