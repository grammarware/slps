#!/usr/bin/python
import sys

#global
emph = [False]
prods = {}

def serialise(name,choices):
 return serialiseX(name,choices)

def mapsymbol(symb):
 if symb[0]=='"':
  return '<bgf:expression><terminal>'+symb[1:-1]+'</terminal></bgf:expression>'
 else:
  return '<bgf:expression><nonterminal>'+symb+'</nonterminal></bgf:expression>'

def map2expr(seq):
 # print 'Mapping',seq,'...'
 if len(seq)==1:
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
     return '<bgf:expression><star>'+map2expr(pseudoseq)+'</star></bgf:expression>'
    line += '<bgf:expression><star>'+map2expr(pseudoseq)+'</star></bgf:expression>'
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
     return '<bgf:expression><optional>'+map2expr(pseudoseq)+'</optional></bgf:expression>'
    line += '<bgf:expression><optional>'+map2expr(pseudoseq)+'</optional></bgf:expression>'
    i = j + 1
   else:
    # regular symbol
    line += mapsymbol(seq[i])
    i += 1
  return line+'</sequence></bgf:expression>'

def serialiseX(name,choices):
 return '<bgf:production><nonterminal>'+name+'</nonterminal>'+traverse(choices)+'</bgf:production>'

def traverse(c):
 if len(c)==1:
  return map2expr(c[0])
 else:
  line = '<bgf:expression><choice>'
  for alt in c:
   line += map2expr(alt)
  return line+'</choice></bgf:expression>'

def addProduction(name,choices):
 bs = []
 for s in range(0,len(choices)):
  ss = []
  for i in range(0,len(choices[s][0])):
   if choices[s][1][i]:
    ss.append(choices[s][0][i])
   else:
    ss.append('"'+choices[s][0][i]+'"')
  bs.append(ss)
 prods[name]=bs

def serialiseT(name,choices):
 line=name+' is defined as:\n'
 for b in choices:
  line += '     '
  for s in b:
   line += s+' '
  line += '\n'
 return line

def addSpaces(line,symb):
 return line.replace(symb,' '+symb+' ')

def preprocess(line):
 l2 = addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(line.strip(),'}'),'{'),'['),']'),')'),'('),';')
 return l2.replace('&gt ; ','&gt;').replace('&lt ; ','&lt;').replace('&amp ; ','&amp;')

def parseLine(line):
 tokens = []
 flags = []
 while line:
  line = line.strip()
  if line.find('</i>')==0:
   emph[0] = False
   line = line[4:]
   continue
  if line.find('<i>')==0:
   emph[0] = True
   line = line[3:]
   continue
  if line.find('</em>')==0:
   emph[0] = False
   line = line[5:]
   continue
  if line.find('<em>')==0:
   emph[0] = True
   line = line[4:]
   continue
  if line.find('<code>')==0:
   emph[0] = False
   line = line[6:]
   continue
  if line.find('</code>')==0:
   emph[0] = True
   line = line[7:]
   continue
  else:
   if line.find('<')>0:
    extra = line[:line.index('<')].strip().split()
    line = line[line.index('<'):]
   else:
    extra = line.strip().split()
    line = ''
   for t in extra:
    tokens.append(t)
    flags.append(emph[0])
 return tokens,flags

def cleanup(line):
 return line.replace('<!-- </i> -->','').replace(' :',':').replace('        ','\t')
 #.replace('<code>','"').replace('</code>','"')

def readGrammar(fn):
 src = open(fn,'r')
 grammar = False
 name = ''
 choices = []
 for line in src:
  if line.find('<pre>')>=0 or line.find('</pre>')>=0:
   if grammar:
    addProduction(name,choices)
   else:
    # dummy parse line for the sake of <i>/<em>
    a,b=parseLine(line.split('<pre>')[1])
   grammar = not grammar
   continue
  if grammar:
   cont = line[0] not in ('\t','<',' ')
   # FIXME!
   #cont = False
   line = preprocess(cleanup(line))
   #print 'Parsing "',line,'"...'
   a,b=parseLine(line)
   if a:
   # non-empty line
    if b[0] and len(a)==1 and a[0][-1]==':':
     # new definition
     if choices:
      # flush the current one
      addProduction(name,choices)
     choices = []
     name = a[0][:-1]
    elif cont:
     # line continuation
     for i in range(0,len(a)):
      choices[-1][0].append(a[i])
      choices[-1][1].append(b[i])
    else:
     # add choice branch
     choices.append([a,b])
 src.close()

def printGrammar(fn):
 ext = open(fn,'w')
 ext.write('<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
 for nt in prods.keys():
  ext.write(serialise(nt,prods[nt]))
 ext.write('</bgf:grammar>')
 ext.close()

def automatedImprove():
 for nt in prods.keys():
  for bs in prods[nt]:
   for i in range(0,len(bs)):
    if bs[i]=='"|"' and len(bs)>1:
     print 'Suspicious context for "|" in',nt+', treating as a BNF bar'
     bs[i] = '|'
    elif bs[i][0]=='"':
     continue
    if bs[i].isalnum():
     if bs[i][0].islower() and (bs[i] not in prods.keys()):
      print 'Cannot find definition for',bs[i]+', treating as a terminal in',nt
      bs[i] = '"'+bs[i]+'"'
    elif bs[i][0] not in ('[',']','{','}','|','(',')'):
     print 'Guessing that "'+bs[i]+'" in',nt,'must be a terminal symbol'
     bs[i] = '"'+bs[i]+'"'
   bs = fixBrackets(nt,bs)
 pass

def fixBrackets(nt,arr):
 if not arr:
  return arr
 arr = fixBracketPair(nt,arr,'(',')')
 arr = fixBracketPair(nt,arr,'[',']')
 arr = fixBracketPair(nt,arr,'{','}')
 return arr

def fixBracketPair(nt,arr,left,right):
 cx = 0
 for el in arr:
  if el==left:
   cx += 1
  if el==right:
   cx -= 1
 if cx==0:
  return arr
 else:
  print 'Bracketing mismatch in',nt,', trying to fix'
  arr.reverse()
  while(cx>0):
   if '"'+right+'"' in arr:
    arr[arr.index('"'+right+'"')]=right
   else:
    arr.remove(left)
   cx -= 1
  arr.reverse()
  while(cx<0):
   if '"'+left+'"' in arr:
    arr[arr.index('"'+left+'"')]=left
   else:
    arr.remove(right)
   cx += 1
  return arr

if __name__ == "__main__":
 print 'HTML to Grammar automated extractor'
 if len(sys.argv) == 3:
  print 'Reading the HTML document...'
  readGrammar(sys.argv[1])
  print 'Massaging the grammar...'
  automatedImprove()
  print 'Writing the extracted grammar...'
  printGrammar(sys.argv[2])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<input>','<output>'
  sys.exit(1)

