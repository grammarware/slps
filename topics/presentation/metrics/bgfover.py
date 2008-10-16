#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

productions = "count(/*/*[local-name()='production'])"
nonterminals = "count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
top = "count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"
bottom = "count(/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"

names   = []
tnames  = []

bgfns = 'http://planet-sl.org/bgf'
ET._namespace_map[bgfns] = 'bgf'

def runxpath(filename,xpathexpr):
 os.system('xpath '+filename+' "'+xpathexpr+'" 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 res = tmp.readline().strip()
 tmp.close()
 return res

if __name__ == "__main__":
 if len(sys.argv) != 3:
  print 'This tool generates an overview of a bunch of BGF sources and targets.'
  print 'Usage:'
  print '      bgfover <lcf> <bgfs-path>'
  sys.exit(1)
 lcf = ET.parse(sys.argv[1])
 for x in lcf.findall('/source'):
  name = x.findtext('name')
  names.append(name)
 srclen = len(names)
 for x in lcf.findall('/target'):
  name = x.findtext('name')
  tnames.append(name)
 path = sys.argv[2]
 if path[-1]!='/':
  path += '/'
 print '\\begin{tabular}{l|c|c|c|c}'
 print '&\\textbf{Total}&\\textbf{Total}&\\textbf{Top}&\\textbf{Bottom}\\\\'
 print '&\\textbf{productions}&\\textbf{nonterminals}&\\textbf{nonterminals}&\\textbf{nonterminals}\\\\\\hline\\hline'
 for x in names:
  print '\\emph{'+x+'}&'+runxpath(path+x+'.bgf',productions)+'&'+runxpath(path+x+'.bgf',nonterminals)+'&'+runxpath(path+x+'.bgf',top)+'&'+runxpath(path+x+'.bgf',bottom)+'\\\\\\hline'
 print '\\hline'
 for x in tnames:
  print '\\emph{'+x+'}&'+runxpath(path+x+'.bgf',productions)+'&'+runxpath(path+x+'.bgf',nonterminals)+'&'+runxpath(path+x+'.bgf',top)+'&'+runxpath(path+x+'.bgf',bottom)+'\\\\\\hline'
 print '\\end{tabular}'
 sys.exit(0)

