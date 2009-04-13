#!/usr/bin/python
import sys
import elementtree.ElementTree as ET

bgfns = 'http://planet-sl.org/bgf'
ET._namespace_map[bgfns] = 'bgf'

if __name__ == "__main__":
 if len(sys.argv) < 2:
  print 'This tool plots a dependency graph of a BGF grammar.'
  print 'Usage:'
  print '       plotbgf <bgf-file> [<ignore-nonterminal> ...]'
  sys.exit(1)
 bgf = ET.parse(sys.argv[1])
 ignore = []
 for int in sys.argv[2:]:
  ignore.append(int)
 depgraph = []
 for p in bgf.findall('//{'+bgfns+'}production'):
  n1 = p.findtext('nonterminal')
  for nt in p.findall('{'+bgfns+'}expression//nonterminal'):
   n2 = nt.text
   if (n1,n2) not in depgraph:
    if n1 not in ignore and n2 not in ignore:
     depgraph.append((n1,n2))
 print 'digraph generated{'
 for ns in depgraph:
  print ns[0],'->',ns[1],';'
 print '}'
 sys.exit(0)
