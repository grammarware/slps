This is an implementation of FL based on the converge language.
The DSL features of converge are put to work.
In particular the grammar of FL is embedded into the program.
Also, the FL tree is translated into converge programs.
This way, an evaluator is obtained.

Tested with Converge current (2008/05/24 21:43:59)
Downloaded at http://convergepl.org
Use "make" to test the application.
"converge" should be in the PATH.
This example has been contributed by Laurence Tratt.
See his email below.
It also explains the original model of this implementation.


---------- Forwarded message ----------
From: Laurence Tratt <laurie@tratt.net>
Date: Sat, May 24, 2008 at 3:45 AM
Subject: FL implementation
To: Ralf Laemmel <rlaemmel@gmail.com>


Dear Ralf,

I saw your FL blog-post this morning and thought it might be a quick fun
example of Converge's DSL features. So please find attached an
implementation which runs against the -current version of Converge to be
found on the website.

When this is run for the first time, it compiles the mini-programs into
Converge functions and shows you what the translation was:

 $ converge -v fl.cv
 ===> Compiling /tmp/fl.cv...
 func mult(n, m):
     return func ():
         if n == 0:
             return 0
         else:
             return m + mult.apply([n - 1, m])()

 func fac(n):
     return func ():
         if n == 0:
             return 1
         else:
             return mult.apply([n, fac.apply([n - 1])])()

 func fib(n):
     return func ():
         if n == 0:
             return 0
         else:
             return func ():
                 if n == 1:
                     return 1
                 else:
                     return fib.apply([n - 1]) + fib.apply([n - 2])()()

 ===> Finished compiling /tmp/fl.cv.
 ===> Linking.
 fac(5) = 120
 fac(10) = 3628800
 fib(10) = 55
 $

On subsequent runs (assuming fl.cv hasn't been changed), it'll just run the
three example function calls. As you can see, I added in the Fibonacci
function. Because I could :)

Please feel free to do with this what you will. If you think it's useful in
the repository, please feel free to include it; equally I won't be offended
if this isn't thought to be a good fit.

Yours,


Laurie
--
http://tratt.net/laurie/ -- Personal
http://convergepl.org/   -- The Converge programming language
