:- ['Evaluation.pro'].

%
% Testing subsititution; see slide 157
%
:- substitute(var(z),x,var(x),T),                     write(T), nl. % var(z)
:- substitute(var(z),x,lam(x,app(var(x),var(x))),T),  write(T), nl. % lam(x,app(var(x),var(x))
:- substitute(var(z),x,lam(y,app(var(y),var(x))),T),  write(T), nl. % lam(y,app(var(y),var(z))

%
% The following test case (taken from a slide of the lecture) requires alpha conversion.
% The trouble is about variable z occurring once freely and once bound.
%
:- substitute(var(z),x,lam(z,app(var(x),var(z))),T),  write(T), nl. % lam(a,app(var(z),var(a))

%
% Testing evaluation; see slide 165
%
:- 
 % Some Church encodings
 True = lam(t,lam(f,var(t))),
%False = lam(t,lam(f,var(f))),
 C0 = lam(s,lam(z,var(z))),
 C1 = lam(s,lam(z,app(var(s),var(z)))),

 % Testing Church conditional (Test)
 Test = lam(l,lam(m,lam(n,app(app(var(l),var(m)),var(n))))),
 manysteps(app(app(app(Test,True),C0),C1),Q1),
 write(Q1), nl,

 % Testing built-in numbers
 manysteps(iszero(pred(1)),Q2),
 write(Q2), nl,

 % Testing built-in numbers and conditional (If)
 If = lam(x,lam(y,if(iszero(var(x)),succ(var(x)),pred(var(y))))),
 manysteps(app(app(If,0),42),Q3),
 write(Q3), nl,

 % Testing recursive functions on Prolog numbers; see slide 190
 % Fix = lam(f,app(lam(x,app(var(f),app(var(x),var(x)))),lam(x,app(var(f),app(var(x),var(x)))))), % CBN
 Fix = lam(f,app(lam(x,app(var(f),lam(y,app(app(var(x),var(x)),var(y))))),lam(x,app(var(f),lam(y,app(app(var(x),var(x)),var(y))))))), % CBV
 F = lam(e,lam(x,if(iszero(var(x)),true,if(iszero(pred(var(x))),false,app(var(e),pred(pred(var(x)))))))),
 IsEven = app(Fix,F),
 manysteps(app(IsEven,3),Q4),
 write(Q4), nl,

 halt.
