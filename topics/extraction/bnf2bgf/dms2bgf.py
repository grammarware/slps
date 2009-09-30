#!/usr/bin/python
# BEWARE OF QUICK AND DIRTY COPY-PASTE PROGRAMMING
import sys

grammar = {}
order = []

def xmlify(s):
 if s == '&':
  return '&amp;'
 else:
  return s.replace('>','&gt;').replace('<','&lt;')

def mapsymbol(symb):
 if symb[0]=="'":
  return '<bgf:expression><terminal>'+xmlify(symb[1:-1])+'</terminal></bgf:expression>'
 else:
  return '<bgf:expression><nonterminal>'+symb+'</nonterminal></bgf:expression>'

def serialiseExpression(seq):
 # print 'Mapping',seq,'...'
 if len(seq)==0:
  return '<bgf:expression><epsilon/></bgf:expression>'
 elif len(seq)==1:
  return mapsymbol(seq[0])
 else:
  line = '<bgf:expression><sequence>'
  i = 0
  while i < len(seq):
   if seq[i]=='(':
    # grouping
    pseudobranch = []
    pseudoroot = []
    j = i + 1
    while True:
     if seq[j]==')':
      pseudoroot.append(pseudobranch)
      break
     elif seq[j]=='|':
      pseudoroot.append(pseudobranch)
      pseudobranch = []
     else:
      pseudobranch.append(seq[j])
     j += 1
    if len(pseudoroot)==1:
     if len(pseudoroot[0])==0:
      i = j + 1
      continue
    line += traverse(pseudoroot)
    i = j + 1
   elif seq[i]=='{':
    # zero or more
    pseudoseq = []
    j = i + 1
    level = 0
    while True:
     if seq[j]=='}':
      if level:
       pseudoseq.append(seq[j])
       level -= 1
      else:
       break
     else:
      pseudoseq.append(seq[j])
     if seq[j]=='{':
      level += 1
     j += 1
    if len(line)==26 and j==len(seq)-1:
     # there is no spoon! I mean, sequence.
     return '<bgf:expression><star>'+serialiseExpression(pseudoseq)+'</star></bgf:expression>'
    line += '<bgf:expression><star>'+serialiseExpression(pseudoseq)+'</star></bgf:expression>'
    i = j + 1
   elif seq[i]=='[':
    # zero or one
    pseudoseq = []
    j = i + 1
    level = 0
    while True:
     if seq[j]==']':
      if level:
       pseudoseq.append(seq[j])
       level -= 1
      else:
       break
     else:
      pseudoseq.append(seq[j])
     if seq[j]=='[':
      level += 1
     j += 1
    if len(line)==26 and j==len(seq)-1:
     # there is no spoon! I mean, sequence.
     return '<bgf:expression><optional>'+serialiseExpression(pseudoseq)+'</optional></bgf:expression>'
    line += '<bgf:expression><optional>'+serialiseExpression(pseudoseq)+'</optional></bgf:expression>'
    i = j + 1
   else:
    # regular symbol
    line += mapsymbol(seq[i])
    i += 1
  return line+'</sequence></bgf:expression>'

def serialiseY(name,choice):
 return '<bgf:production><nonterminal>'+name+'</nonterminal>'+serialiseExpression(choice)+'</bgf:production>'

def printGrammarVertical(fn):
 ext = open(fn,'w')
 ext.write('<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
 for nt in order:
  for prod in grammar[nt]:
   ext.write(serialiseY(nt,prod))
 ext.write('</bgf:grammar>')
 ext.close()

def dms2lines(f):
 dms = open(f,'r')
 bugs = goods = 0
 for line in dms.readlines():
  # hacked up! need own strip for robustness later
  words = line.strip().split()
  if not words:
   continue
  if words[0] == '[':
   words = words[words.index(']')+1:]
  if words[1] != '=':
   #print '[----] No equals sign!'
   bugs += 1
  elif words[-1] != ';':
   #print '[----] No semicolon!'
   bugs += 1
  elif words[2][0] == ':':
   #print '[----] Lexical!'
   bugs += 1
  else:
   if words[0] not in grammar.keys():
    grammar[words[0]] = []
    order.append(words[0])
   grammar[words[0]].append(words[2:-1])
   goods += 1
 dms.close()
 return bugs, goods

if __name__ == "__main__":
 print 'DMS BNF to Grammar automated extractor'
 if len(sys.argv) == 3:
  print 'Reading the BNF...'
  a,b = dms2lines(sys.argv[1])
  print 'Skipped',a,'productions, parsed',b,'productions.'
  #print 'Massaging the grammar...'
  print 'Writing the extracted grammar...'
  printGrammarVertical(sys.argv[2])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'''<input> <output>'''
  sys.exit(1)
