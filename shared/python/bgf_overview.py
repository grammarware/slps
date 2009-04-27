#!/usr/bin/python
import os
import sys
import slpsXPath
import slpsns
import elementtree.ElementTree as ET

names   = []

if __name__ == "__main__":
 if len(sys.argv) != 4:
  print 'This tool generates an overview of a bunch of BGF sources and targets.'
  print 'Usage:'
  print '      bgfover <lcf> <bgfs-path> <xpath>'
  sys.exit(1)
 lcf = ET.parse(sys.argv[1])
 for x in lcf.findall(sys.argv[3]):
  name = x.findtext('name')
  names.append(name)
 path = sys.argv[2]
 if path[-1]!='/':
  path += '/'
 print '''\\begin{tabular}{l|c|c|c|c}
&\\numberOfProductions
&\\numberOfNonterminals
&\\numberOfTops
&\\numberOfBottoms
\\\\\\hline\\hline
'''
 for x in names:
  print '\\emph{'+x+'}&'+slpsXPath.runxpath(path+x+'.bgf',slpsXPath.productions)+'&'+slpsXPath.runxpath(path+x+'.bgf',slpsXPath.nonterminals)+'&'+slpsXPath.runxpath(path+x+'.bgf',slpsXPath.top)+'&'+slpsXPath.runxpath2(path+x+'.bgf',slpsXPath.bottom)+'\\\\\\hline'
 print '\\end{tabular}'
 sys.exit(0)
