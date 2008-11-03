#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

safexbgf = ('eliminate','introduce','chain','designate','deyaccify','distribute','extract','factor','fold','horizontal','inline','massage','rename','reroot','strip','unfold','vertical','yaccify')
incdecxbgf = ('add','narrow','remove','unite','widen')
rkeys = ('LOC','NOI','NOX','NI~','NI+','NI!','SGO','COR','NI^','SID','SRE')

names   = []
targets = {}
results = {}

bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
xsdns = 'http://www.w3.org/2001/XMLSchema'

ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
ET._namespace_map[xsdns] = 'xsd'

def noni(filename,arrayxbgf):
 global xbgfns
 cx = 0
 xbgf = ET.parse(filename)
 for c in arrayxbgf:
  inc = len(xbgf.findall('//{'+xbgfns+'}'+c))
  if inc:
   #print 'Found',c
   pass
  cx += inc
 return cx

def loc(filename):
 f = open(filename,'r')
 c = len(f.readlines())
 f.close()
 return c-1

def nosi(filename,keyword):
 f = open(filename,'r')
 c = ''.join(f.readlines()).count(keyword)
 f.close()
 return c

def noi(filename):
 f = open(filename,'r')
 c = ''.join(f.readlines()).count('<!--')
 f.close()
 return c

def report(keys,key,note):
 print note,
 cx = 0
 for x in keys:
  cx += results[key][x]
  if results[key][x]:
   print '{',results[key][x],'}'
  else:
   print '{---}',
 print '{'+`cx`+'}'

if __name__ == "__main__":
 if len(sys.argv) != 2:
  print 'This tool generates an overview of an XBGF script.'
  print 'Usage:'
  print '      xi <file>'
  sys.exit(1)
 xbgf = ET.parse(sys.argv[1])
 print "%25s" % sys.argv[1].split('.')[0]+':',
 cx1 = noni(sys.argv[1],safexbgf)   + nosi(sys.argv[1],'BREFACTOR')
 cx2 = noni(sys.argv[1],incdecxbgf) + nosi(sys.argv[1],'GENERALITY')
 cx3 = nosi(sys.argv[1],'REVISE')
 sum = len(xbgf.findall('/*'))
 print "%3i +%3i +%3i =%4i" % (cx1,cx2,cx3,sum),
 if cx1+cx2+cx3!=sum:
  print 'ERROR'
  sys.exit(1)
 else:
  print
  sys.exit(0)

