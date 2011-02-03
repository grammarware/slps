#!/usr/bin/python
import os
import sys
import string
import elementtree.ElementTree as ET
import slpsns
import mismatches

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

def main(lcffile,gdt,prefix):
 ltree = ET.parse(lcffile)
 newLcf = ET.Element(slpsns.lcf_('configuration'))
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
   for p in b.findall('preparation/perform'):
    start += '.'+cutName(p.text)
   for p in b.findall('nominal-matching/perform'):
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
   prev,csvstr = measure(gdt,shorten(start),t.findtext('name'))
   csv.write('initial\t'+csvstr+'\n')
   for p in b.findall('*/perform'):
    start += '.'+cutName(p.text)
    cur,csvstr = measure(gdt,shorten(start),t.findtext('name'))
    if cur>prev:
     csvstr += '\tERROR'
    csv.write(p.text+'\t'+csvstr+'\n')    
    prev = cur
   csv.close()
  print 'Target',t.findtext('name'),'done.'
 #ET.ElementTree(newLcf).write(lcfName)
 return

def measure(gdt,x,y):
 if synch[y][0].split('.')[0]!=x.split('.')[0]:
  z=synch[y][0]
 else:
  z=synch[y][1]
 nm,sm = mismatches.mismatches(gdt,'bgf/'+x+'.bgf','bgf/'+z+'.bgf')
 return nm+sm,str(nm)+'\t'+str(sm)+'\t'+str(nm+sm)

def cutName(lbl):
 l=''
 for x in lbl:
  if x.islower() or x=='.':
   l+=x
  else:
   break
 return l

if __name__ == "__main__":
 if len(sys.argv) == 4:
  slpsns.init(ET)
  apply(main,sys.argv[1:4])
 else:
  print '''This tool takes an LCF file and produces a set of CSV files, diffing all BGFs in all branches

Usage:'''
  print ' ',sys.argv[0],'<input lcf file>','<comparator>','<prefix>'
  sys.exit(1)
