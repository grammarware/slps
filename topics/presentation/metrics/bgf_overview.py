#!/usr/bin/python
import os
import sys
import elementtree.ElementTree as ET

productions = "count(/*/*[local-name()='production'])"
nonterminals = "count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
top = "count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"
oldbottom = "count(/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
bottom = " | (echo '<set>' ; xpath '//nonterminal[not(text()=/*/*/nonterminal/text())]' 2> /dev/null; echo '</set>' )| xpath 'count(//nonterminal[not(text()=preceding-sibling::*/text())])' "

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

def runxpath2(filename,xpathexpr):
 os.system('cat '+filename+xpathexpr+' 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 res = tmp.readline().strip()
 tmp.close()
 return res

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
 #print '\\begin{tabular}{l|c|c|c|c}'
 #print '&\\textbf{Total}&\\textbf{Total}&\\textbf{Top}&\\textbf{Bottom}\\\\'
 #print '&\\textbf{productions}&\\textbf{nonterminals}&\\textbf{nonterminals}&\\textbf{nonterminals}\\\\\\hline\\hline'
 print '''\\begin{tabular}{l|c|c|c|c}
&\\numberOfProductions
&\\numberOfNonterminals
&\\numberOfTops
&\\numberOfBottoms
\\\\\\hline\\hline
'''
 for x in names:
  print '\\emph{'+x+'}&'+runxpath(path+x+'.bgf',productions)+'&'+runxpath(path+x+'.bgf',nonterminals)+'&'+runxpath(path+x+'.bgf',top)+'&'+runxpath2(path+x+'.bgf',bottom)+'\\\\\\hline'
 print '\\end{tabular}'
 sys.exit(0)

