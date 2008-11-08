#!/usr/bin/python
import os
import sys

productions = "count(/*/*[local-name()='production'])"
nonterminals = "count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
top = "count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"
listtop = "/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())]"
bottom = " | (echo '<set>' ; xpath '//nonterminal[not(text()=/*/*/nonterminal/text())]' 2> /dev/null; echo '</set>' )| xpath 'count(//nonterminal[not(text()=preceding-sibling::*/text())])' "
listbottom = "//nonterminal[not(text()=/*/*/nonterminal/text())]"
unique = " | (echo '<set>' ; xpath '//nonterminal' 2> /dev/null; echo '</set>' )| xpath 'count(//nonterminal[not(text()=preceding-sibling::*/text())])' "

def runxpath(filename,xpathexpr):
 os.system('xpath '+filename+' "'+xpathexpr+'" 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 res = tmp.readline().strip()
 tmp.close()
 return res

def runxpathlist(filename,xpathexpr):
 os.system('xpath '+filename+' "'+xpathexpr+'" 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 resa = tmp.readline().strip().split('<nonterminal>')
 resu = []
 for x in resa:
  if x not in resu:
   resu.append(x)
 resu.sort()
 return ''.join(resu).replace('</nonterminal>',', ')[:-2]
 tmp.close()
 res = res.replace('<nonterminal>','').replace('</nonterminal>',', ')
 return res[:-2]

def runxpath2(filename,xpathexpr):
 os.system('cat '+filename+xpathexpr+' 1>TMP-res 2>/dev/null')
 tmp = open('TMP-res','r')
 res = tmp.readline().strip()
 tmp.close()
 return res

if __name__ == "__main__":
 if len(sys.argv) != 2:
  print 'This tool generates an overview of a bunch of BGF sources and targets.'
  print 'Usage:'
  print '      checkbgf <bgf>'
  sys.exit(1)
 print 'Productions:         ',runxpath(sys.argv[1],productions)
 print 'Nonterminals defined:',runxpath(sys.argv[1],nonterminals)
 print '%20s:' % 'unique',runxpath2(sys.argv[1],unique)
 print '%20s:' % 'bottom',runxpath2(sys.argv[1],bottom),'(',runxpathlist(sys.argv[1],listbottom),')'
 print '%20s:' % 'top',runxpath(sys.argv[1],top),'(',runxpathlist(sys.argv[1],listtop),')'
 sys.exit(0)

