#!/usr/bin/python
import os
import slpsns

productions = "count(/*/*[local-name()='production'])"
nonterminals = "count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
top = "count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"
listtop = "/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())]"
oldbottom = "count(/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"
bottom = " | (echo '<set>' ; xpath '//nonterminal[not(text()=/*/*/nonterminal/text())]' 2> /dev/null; echo '</set>' )| xpath 'count(//nonterminal[not(text()=preceding-sibling::*/text())])' "
listbottom = "//nonterminal[not(text()=/*/*/nonterminal/text())]"
unique = " | (echo '<set>' ; xpath '//nonterminal' 2> /dev/null; echo '</set>' )| xpath 'count(//nonterminal[not(text()=preceding-sibling::*/text())])' "

safexbgf = ('abridge','detour','anonymize','deanonymize','unlabel','designate',
            'deyaccify','yaccify','eliminate','introduce','inline','extract',
            'unfold','fold','horizontal','vertical','distribute','factor',
            'massage','chain','unchain','skip','reroot','import')
#safexbgf =   ('eliminate','introduce','chain', 'designate', 'deyaccify','distribute',
#              'extract',  'factor',   'fold',  'horizontal','inline',   'massage',
#              'rename',   'reroot',   'unfold','vertical',  'yaccify',  'unchain',   'skip')
incdecxbgf = ('remove','add','disappear','appear','narrow','widen',
              'downgrade','upgrade','rassoc','lassoc','unite')
#incdecxbgf = ('add',      'narrow',   'remove','unite',     'widen',    'rassoc',    'lassoc')
messyxbgf =  ('permute',  'dump','project','inject','concretize','abstractize')

rkeys = ('LOC','NOI','NOX','NI~','NI+','NI!','SGO','COR','NI^','SID','SRE','EKB','EPX','EIC','FRE','FEX','FCO','EAR','FIN')

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

def notr(xbgf):
 return len(xbgf.findall('/*')) - \
        len(xbgf.findall('/'+slpsns.xbgf_('atomic'))) + \
        len(xbgf.findall('/'+slpsns.xbgf_('atomic')+'/*'))

def noni(xbgf,arrayxbgf):
 cx = 0
 for c in arrayxbgf:
  cx += len(xbgf.findall('//'+slpsns.xbgf_(c)))# + len(xbgf.findall('//'+slpsns.xbgf_('atomic')+'/'+slpsns.xbgf_(c)))
 return cx

def noPartiallyUnsafe(xbgf):
 return len(xbgf.findall('//'+slpsns.xbgf_('strip/terminal')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('strip/allTerminals')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('rename/terminal')))

def noPartiallySafe(xbgf):
 return len(xbgf.findall('//'+slpsns.xbgf_('strip/selector')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('strip/allSelectors')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('strip/label')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('strip/allLabels')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('rename/nonterminal')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('rename/selector')))+\
        len(xbgf.findall('//'+slpsns.xbgf_('rename/label')))

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

def countSemanticPreserving(xbgf,xbgfFile):
 return noni(xbgf,safexbgf) + nosi(xbgfFile,'BREFACTOR') + noPartiallySafe(xbgf)

def countSemanticIncDec(xbgf,xbgfFile):
 return noni(xbgf,incdecxbgf) + nosi(xbgfFile,'GENERALITY') - nosi(xbgfFile,'NOTINCDEC')

def countSemanticRevising(xbgf,xbgfFile):
 return noni(xbgf,messyxbgf) + nosi(xbgfFile,'REVISE') + noPartiallyUnsafe(xbgf)

