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
   for p in b.findall('postextraction/perform'):
    start += '.'+cutName(p.text)
   for p in b.findall('synchronization/perform'):
    start += '.'+cutName(p.text)
   for p in b.findall('normalization/perform'):
    start += '.'+cutName(p.text)
   synch[t.findtext('name')].append(shorten(start))
 #print synch
 # generate csvs
 for t in ltree.findall('target'):
  for b in t.findall('branch'):
   start = b.findtext('input')
   csv = open(prefix+'.'+t.findtext('name')+'.'+start+'.txt','w')
   csv.write('initial\t'+measure(shorten(start),t.findtext('name'))+'\n')
   for p in b.findall('*/perform'):
    start += '.'+cutName(p.text)
    csv.write(p.text+'\t'+measure(shorten(start),t.findtext('name'))+'\n')    
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
 return nameDiffs+'\t'+str(strDiffs)+'\t'+str(int(nameDiffs)+strDiffs)

def cutName(lbl):
 l=''
 for x in lbl:
  if x.islower() or x=='.':
   l+=x
  else:
   break
 return l

def forNewTarget(xbgfDir1,xbgfDir2,t):
 print 'Splitting target',t.findtext('name'),'...'
 nt = ET.Element('target')
 nt.append(t.findall('name')[0])
 for branch in t.findall('branch'):
  nbr = ET.Element('branch')
  for e in branch.findall('*'):
   if e.tag == 'input':
    nbr.append(e)
   else:
    phase = ET.Element(e.tag)
    for step in e.findall('*'):
     # perform or automated
     print '-->',step.tag,
     if step.tag=='perform':
      print step.text,'-',
      for s in sliceFile(xbgfDir1,xbgfDir2,step.text):
       phase.append(s)
     else:
      print step.findtext('result'),'-',
      for s in sliceFile(xbgfDir1,xbgfDir2,step.findtext('result')):
       phase.append(s)
     #phase.append(step)
    nbr.append(phase)
  nt.append(nbr)
 return nt

def sliceFile(xbgfDir1,xbgfDir2,text):
 sliced = []
 xtree = ET.parse(xbgfDir1+text+'.xbgf')
 cx = 0
 for t in xtree.findall('*'):
  cx += 1
  seq = ET.Element('{'+xbgfns+'}sequence')
  if t.tag=='{'+xbgfns+'}atomic':
   for sub in t.findall('*'):
    seq.append(sub)
  else:
   seq.append(t)
  ET.ElementTree(seq).write(xbgfDir2+text+'-'+`cx`+'.xbgf')
  p = ET.Element('perform')
  p.text = text+'-'+`cx`
  sliced.append(p)
 print cx,'slices'
 return sliced

if __name__ == "__main__":
 if len(sys.argv) == 3:
  apply(main,sys.argv[1:3])
 else:
  print '''This tool takes an LCF file and produces a set of CSV files, diffing all BGFs in all branches

Usage:'''
  print ' ',sys.argv[0],'<input lcf file>','<prefix>'
  sys.exit(1)
