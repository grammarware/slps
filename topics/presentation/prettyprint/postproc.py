#!/usr/bin/python
import sys

mode = 0
for line in sys.stdin:
 if mode == 1:
  if line.find('\\end{lstlisting}\\end{graybox}')>-1:
   mode = 0
   lline = line
  else:
   tokens = line.split(' ')
   lline = tokens[0]
   for token in tokens[1:]:
    if len(lline + ' ' + token)>55:
     print lline
     lline = ' '*16 + token
    else:
     lline = lline + ' ' + token
  print lline,
 elif mode == 0:
  if line.find('\\begin{lstlisting}[language=pp]')>-1:
   mode = 1
  print line,
