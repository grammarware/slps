#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

productions = "count(/*/*[local-name()='production'])"
nonterminals = "count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
top = "count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"
bottom = "count(/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"

names   = []

bgfns = 'http://planet-sl.org/bgf'
ET._namespace_map[bgfns] = 'bgf'

def runxpath(filename,xpathexpr):
 os.system('xpath '+filename+' "'+xpathexpr+'" 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 res = tmp.readline().strip()
 tmp.close()
 return res

def fillinline(path,xexpr):
 s = ''
 for x in names:
  s += '&'+runxpath(path+x+'.bgf',xexpr)
 return s+'\\\\'

if __name__ == "__main__":
 if len(sys.argv) != 3:
  print 'This tool generates an overview of a bunch of BGF sources.'
  print 'Usage:'
  print '      xbgfover <lcf> <bgfs-path>'
  sys.exit(1)
 lcf = ET.parse(sys.argv[1])
 for x in lcf.findall('/source'):
  name = x.findtext('name')
  names.append(name)
 srclen = len(names)
 for x in lcf.findall('/target'):
  name = x.findtext('name')
  names.append(name)
 path = sys.argv[2]
 if path[-1]!='/':
  path += '/'
 print '\\begin{tabular}{l|'+('c|'*srclen)+'|'+('c|'*(len(names)-srclen))+'}'
 for x in names:
  print '&\\textbf{'+x+'}'
 print '\\\\\\hline'
 print 'Total productions:  ',fillinline(path,productions)
 print 'Total nonterminals: ',fillinline(path,nonterminals)
 print 'Top nonterminals:   ',fillinline(path,top)
 print 'Bottom nonterminals:',fillinline(path,bottom)
 print '\\hline'
 print '\\end{tabular}'
 sys.exit(0)

