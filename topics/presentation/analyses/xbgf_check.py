#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

safexbgf = ('abridge','detour','anonymize','deanonymize','unlabel','designate',
            'deyaccify','yaccify','eliminate','introduce','inline','extract',
            'unfold','fold','horizontal','vertical','distribute','factor',
            'massage','chain','unchain','skip','reroot','rename')
#safexbgf =   ('eliminate','introduce','chain', 'designate', 'deyaccify','distribute',
#              'extract',  'factor',   'fold',  'horizontal','inline',   'massage',
#              'rename',   'reroot',   'unfold','vertical',  'yaccify',  'unchain',   'skip')
incdecxbgf = ('remove','add','disappear','appear','narrow','widen',
              'downgrade','upgrade','rassoc','lassoc','unite')
#incdecxbgf = ('add',      'narrow',   'remove','unite',     'widen',    'rassoc',    'lassoc')
messyxbgf =  ('permute',  'dump')

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

def noni(xbgf,arrayxbgf):
 global xbgfns
 cx = 0
 for c in arrayxbgf:
  cx += len(xbgf.findall('//{'+xbgfns+'}'+c))
 return cx
 
def nostripunsafe(xbgf):
 global xbgfns
 return len(xbgf.findall('//{'+xbgfns+'}strip/terminal'))+\
        len(xbgf.findall('//{'+xbgfns+'}strip/allTerminals'))

def nostripsafe(xbgf):
 global xbgfns
 return len(xbgf.findall('//{'+xbgfns+'}strip/selector'))+\
        len(xbgf.findall('//{'+xbgfns+'}strip/allSelectors'))+\
        len(xbgf.findall('//{'+xbgfns+'}strip/label'))+\
        len(xbgf.findall('//{'+xbgfns+'}strip/allLabels'))

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
  print '      checkxbgf <file>'
  sys.exit(1)
 xbgf = ET.parse(sys.argv[1])
 print "%25s" % sys.argv[1].split('.')[0]+':',
 cx1 = noni(xbgf,safexbgf)   + nosi(sys.argv[1],'BREFACTOR')  + nostripsafe(xbgf)
 cx2 = noni(xbgf,incdecxbgf) + nosi(sys.argv[1],'GENERALITY')
 cx3 = noni(xbgf,messyxbgf)  + nosi(sys.argv[1],'REVISE')     + nostripunsafe(xbgf)
 sum = len(xbgf.findall('/*'))
 print "%3i +%3i +%3i =%4i" % (cx1,cx2,cx3,sum),
 if cx1+cx2+cx3!=sum:
  print 'ERROR'
  sys.exit(1)
 else:
  print
  sys.exit(0)

