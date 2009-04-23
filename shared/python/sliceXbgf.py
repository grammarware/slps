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

def main(lcffile,outdir):
 ltree = ET.parse(lcffile)
 newLcf = ET.Element('{'+lcfns+'}configuration')
 dirName = outdir
 if dirName[-1]!='/':
  dirName += '/'
 lcfName = dirName+lcffile.split('/')[-1]
 xbgfDir = '/'.join(lcffile.split('/')[:-1])
 if xbgfDir[-1]=='/':
  xbgfDir += 'xbgf/'
 else:
  xbgfDir += '/xbgf/'
 for el in ltree.findall('*'):
  if el.tag == 'target':
   newLcf.append(forNewTarget(xbgfDir,dirName+'xbgf/',el))
  else:
   newLcf.append(el)
 ET.ElementTree(newLcf).write(lcfName)
 return

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
  print '''XBGF slicing tool

Usage:'''
  print ' ',sys.argv[0],'<input lcf file>','<output directory>'
  sys.exit(1)
