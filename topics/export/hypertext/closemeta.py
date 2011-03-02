#!/usr/bin/python
import sys

for line in sys.stdin:
 if (line.find('<meta')<0):
  print line,
 else:
  print line.replace('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">','<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>'),

