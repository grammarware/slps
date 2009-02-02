#!/usr/bin/python
import sys

if __name__ == "__main__":
 if len(sys.argv)!=2:
  print 'Horizontal productions detector for BNF - please provide a file name.'
  sys.exit(1)
 file = open(sys.argv[1],'r')
 state = 1
 name = 'UNDEFINED'
 for line in file.readlines():
  if line[0] in (' ','\t'):
   if state==1:
    state = 2
   elif state==2:
    print name
    state = 3
  else:
   name = line.strip()[:-1]
   state = 1
 file.close()

