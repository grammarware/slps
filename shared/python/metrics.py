#!/usr/bin/python
import sys

def mismatches(comparator,x,y):
 if synch[y][0].split('.')[0]!=x.split('.')[0]:
  z=synch[y][0]
 else:
  z=synch[y][1]
 #print '[',y,']',x,'vs',z
 run = comparator+' bgf/'+x+'.bgf bgf/'+z+'.bgf | grep "only:" | grep -o "\[..*\]" | wc -w'
 if os.system(run+' > TMP-res'):
  nameDiffs = '0'
  #print 'ERROR1:',run
 else:
  num = open('TMP-res','r')
  nameDiffs = num.readline().strip()
  num.close()
 run = comparator+' bgf/'+x+'.bgf bgf/'+z+'.bgf | grep Fail'
 if os.system(run+' > TMP-res'):
  strDiffs = 0
  #print 'ERROR2:',run
 else:
  num = open('TMP-res','r')
  strDiffs = 0
  for line in num.readlines():
   nsn = line.strip().split('(')[1].split(')')[0].split('/')
   strDiffs += max(int(nsn[0]),int(nsn[1]))
  num.close()
 return int(nameDiffs),strDiffs
 #,nameDiffs+'\t'+str(strDiffs)+'\t'+str(int(nameDiffs)+strDiffs)
