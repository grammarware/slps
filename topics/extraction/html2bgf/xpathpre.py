#!/usr/bin/python
import sys

f = open(sys.argv[1],'r')
grammar = False
print '<pre>'
for chunk in ''.join(f.readlines()).split('<pre>'):
 if chunk.find('19.2')>0:
  grammar = True
 elif grammar:
  print chunk.split('</pre>')[0].replace('<br>','').replace('&#32;',' ')
print '</pre>'
f.close()
