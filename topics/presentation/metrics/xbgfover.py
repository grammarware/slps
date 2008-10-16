#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

names   = []
targets = {}
results = {}

bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
xsdns = 'http://www.w3.org/2001/XMLSchema'

ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
ET._namespace_map[xsdns] = 'xsd'

def loc(filename):
 f = open(filename,'r')
 c = len(f.readlines())
 f.close()
 return c-1

def noi(filename):
 f = open(filename,'r')
 c = ''.join(f.readlines()).count('<!--')
 f.close()
 return c

def report(keys,key,note):
 print note,
 for x in keys:
  if results[key][x]:
   print '&',results[key][x],
  else:
   print '& ---',
 print '\\\\'

if __name__ == "__main__":
 if len(sys.argv) != 4:
  print 'This tool generates an overview of a bunch of XBGF scripts.'
  print 'Usage:'
  print '      xbgfover <xbgf.xsd> <lcf> <xbgfs-path>'
  sys.exit(1)
 xsd = ET.parse(sys.argv[1])
 for x in xsd.findall('/*'):
  if x.attrib.has_key('name'):
   names.append(x.attrib['name'])
 lcf = ET.parse(sys.argv[2])
 for x in lcf.findall('/target'):
  name = x.findtext('name')
  targets[name] = []
  for y in x.findall('branch/perform'):
   targets[name].append(y.text)
 path = sys.argv[3]
 if path[-1] != '/':
  path += '/'
 results['LOC'] = {}
 results['NOI'] = {}
 results['NOX'] = {}
 for x in names:
  results[x] = {}
  for y in targets.keys():
   results[x][y] = 0
 for x in targets.keys():
  results['LOC'][x] = 0
  results['NOI'][x] = 0
  results['NOX'][x] = 0
  for y in targets[x]:
   results['LOC'][x] += loc(path+y+'.xbgf')
   results['NOI'][x] += noi(path+y+'.xbgf')
   xbgf = ET.parse(path+y+'.xbgf')
   results['NOX'][x] += len(xbgf.findall('/*'))
   for z in names:
    results[z][x] += len(xbgf.findall('/{'+xbgfns+'}'+z))
 for x in names[:]:
  used = False
  for y in targets.keys():
   if results[x][y]:
    used = True
  if not used:
   print '%%',x,'not used in any XBGF script'
   names.remove(x)
 sorted = targets.keys()[:]
 sorted.sort()
 print '\\begin{tabular}{l|'+('c|'*len(targets))+'}'
 for x in sorted:
  print '&\\textbf{'+x+'}',
 print '\\\\\\hline'
 report(sorted,'LOC','\\numberOfLines')
 print '\\numberOfSteps',
 for x in sorted:
  print '&',len(targets[x]),
 print '\\\\'
 report(sorted,'NOI','\\numberOfIssues')
 report(sorted,'NOX','\\numberOfTransformations')
 print '\\hline'
 for x in names:
  report(sorted,x,'\\xbgfNumber{'+x+'}')
 print '\\hline'
 print '\\end{tabular}'
 sys.exit(0)

