#!/usr/bin/python
import os
import sys
import slpsXPath

if __name__ == "__main__":
 if len(sys.argv) != 2:
  print 'This tool generates an overview of a BGF file.'
  print 'Usage:'
  print '      checkbgf <bgf>'
  sys.exit(1)
 print 'Productions:         ',slpsXPath.runxpath(sys.argv[1],slpsXPath.productions)
 print 'Nonterminals defined:',slpsXPath.runxpath(sys.argv[1],slpsXPath.nonterminals)
 print '%20s:' % 'unique',slpsXPath.runxpath2(sys.argv[1],slpsXPath.unique)
 print '%20s:' % 'bottom',slpsXPath.runxpath2(sys.argv[1],slpsXPath.bottom),'(',slpsXPath.runxpathlist(sys.argv[1],slpsXPath.listbottom),')'
 print '%20s:' % 'top',slpsXPath.runxpath(sys.argv[1],slpsXPath.top),'(',slpsXPath.runxpathlist(sys.argv[1],slpsXPath.listtop),')'
 sys.exit(0)
