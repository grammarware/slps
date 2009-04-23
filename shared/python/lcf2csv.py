#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET

lcfns = 'http://planet-sl.org/lcf'
ldfns = 'http://planet-sl.org/ldf'
xldfns = 'http://planet-sl.org/xldf'
bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
xsdns = 'http://www.w3.org/2001/XMLSchema'
htmlns= 'http://www.w3.org/1999/xhtml'

ET._namespace_map[lcfns] = 'lcf'
ET._namespace_map[ldfns] = 'ldf'
ET._namespace_map[xldfns] = 'xldf'
ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
ET._namespace_map[xsdns] = 'xsd'
ET._namespace_map[htmlns]='html'

synch = {}

def shorten(dottedName):
 array = dottedName.split('.')
 name = [array[0]]
 for a in array[1:]:
  if name[-1].isdigit():
   if name[-2]==cutName(a):
    name[-1] = str(int(name[-1])+1)
   else:
    name.append(cutName(a))
  elif name[-1]==cutName(a):
   name.append('2')
  else:
   name.append(cutName(a))
 return '.'.join(name)

def main(lcffile,prefix):
 ltree = ET.parse(lcffile)
 newLcf = ET.Element('{'+lcfns+'}configuration')
 xbgfDir = '/'.join(lcffile.split('/')[:-1])
 if xbgfDir == '':
  xbgfDir = 'xbgf/'
 elif xbgfDir[-1]=='/':
  xbgfDir += 'xbgf/'
 else:
  xbgfDir += '/xbgf/'
 # find synch points
 for t in ltree.findall('target'):
  synch[t.findtext('name')] = []
  for b in t.findall('branch'):
   start = b.findtext('input')
   for p in b.findall('earlyfixes/perform'):
    start += '.'+cutName(p.text)
   for p in b.findall('namematching/perform'):
    start += '.'+cutName(p.text)
   for p in b.findall('normalizing/perform'):
    start += '.'+cutName(p.text)
   synch[t.findtext('name')].append(shorten(start))
 #print synch
 # generate csvs
 for t in ltree.findall('target'):
  for b in t.findall('branch'):
   start = b.findtext('input')
   csv = open(prefix+'.'+t.findtext('name')+'.'+start+'.txt','w')
   prev,csvstr = measure(shorten(start),t.findtext('name'))
   csv.write('initial\t'+csvstr+'\n')
   for p in b.findall('*/perform'):
    start += '.'+cutName(p.text)
    cur,csvstr = measure(shorten(start),t.findtext('name'))
    if cur>prev:
     csvstr += '\tERROR'
    csv.write(p.text+'\t'+csvstr+'\n')    
    prev = cur
   csv.close()
  print 'Target',t.findtext('name'),'done.'
 #ET.ElementTree(newLcf).write(lcfName)
 return

def measure(x,y):
 if synch[y][0].split('.')[0]!=x.split('.')[0]:
  z=synch[y][0]
 else:
  z=synch[y][1]
 #print '[',y,']',x,'vs',z
 run = 'gdt bgf/'+x+'.bgf bgf/'+z+'.bgf | grep "only:" | grep -o "\[..*\]" | wc -w'
 if os.system(run+' > TMP-res'):
  nameDiffs = '0'
  #print 'ERROR1:',run
 else:
  num = open('TMP-res','r')
  nameDiffs = num.readline().strip()
  num.close()
 run = 'gdt bgf/'+x+'.bgf bgf/'+z+'.bgf | grep Fail'
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
 return int(nameDiffs)+strDiffs,nameDiffs+'\t'+str(strDiffs)+'\t'+str(int(nameDiffs)+strDiffs)

def cutName(lbl):
 l=''
 for x in lbl:
  if x.islower() or x=='.':
   l+=x
  else:
   break
 return l

if __name__ == "__main__":
 if len(sys.argv) == 3:
  apply(main,sys.argv[1:3])
 else:
  print '''This tool takes an LCF file and produces a set of CSV files, diffing all BGFs in all branches

Usage:'''
  print ' ',sys.argv[0],'<input lcf file>','<prefix>'
  sys.exit(1)
