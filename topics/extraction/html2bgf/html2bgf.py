#!/usr/bin/python
import sys

#global
emph = [False]
pessimistic = [False,0,0]
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

def addProduction(name,choices,oneof):
 bs = []
 if oneof:
  # concatenate all choices
  for c in choices:
   for s in c[0]:
    bs.append(['"'+s+'"'])
 else:
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
 l2 = addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(addSpaces(line.strip(),'}'),'{'),'['),']'),')'),'('),';'),':')
 return l2.replace('&gt ; ','&gt;').replace('&lt ; ','&lt;').replace('&amp ; ','&amp;')

def parseLine(line):
 oldline = line[:]
 tokens = []
 flags = []
 while line:
  line = line.strip()
  if pessimistic[0]:
   if line=='<hr>':
    pessimistic[0]=False
   line = ''
   continue
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
   if emph[0] and tokens and oldline.find(tokens[-1]+'<em>'+line[4:line.index('>')])>=0:
    print 'Token-breaking <em> tag endangers',
    line = tokens.pop()+line[4:]
    print line.split()[0].split('<')[0]
    flags.pop()
   else:
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
  if line.find('<sub><i>opt</i></sub>')==0:
   tokens.append('?????')
   flags.append(True)
   line = line[21:]
   continue
  if line.find('<sub>opt</sub>')==0:
   tokens.append('?????')
   flags.append(True)
   line = line[14:]
   continue
  if line.find('<sub><i>opt')==0:
   tokens.append('?????')
   flags.append(True)
   #last = tokens.pop()
   #lastf = flags.pop()
   #tokens.extend(['[',last,']'])
   #flags.extend([True,lastf,True])
   line = line[11:]
   continue
  if line.find('</sub>')==0:
   line = line[6:]
   continue
  if line.find('<sub>')==0:
   line = line[5:]
   continue
  if line.find('<hr>')==0:
   line = line.replace('<hr>','')
   pessimistic[0] = False
   continue
  if line.find('<a')==0:
   #print 'Anchor found, skipping everything that is left of this snippet.'
   pessimistic[0] = True
   pessimistic[1] += 1
   continue
  if line.find('<')==0:
   print 'Found unknown tag while parsing "'+line+'", skipping!'
   pessimistic[2] += 1
   line = line[line.index('>')+1:]
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
 return line.replace('<!-- </i> -->','').replace('        ','\t')
 #.replace('<code>','"').replace('</code>','"')

def ifContinuation(s,olds):
 if not s:
  return False
 if s[0]=='\t' and (s[1]!='\t' or s[1]==olds[1]):
  return False
 if s[0]==' ':
  i=0
  while s[i]==' ' and olds[i]==' ':
   i+=1
  if olds[i]!=' ' and s[i]==' ' and s[i+1]!=' ':
   # one space indentation equals line continuation
   return True
  return False
 if s[0]=='<':
  return ifContinuation(s[s.index('>')+1:],olds)
 return True

def readGrammar(fn):
 oneof = False
 src = open(fn,'r')
 grammar = False
 name = ''
 oldline = ''
 choices = []
 for line in src:
  if line.find('<pre>')>=0 or line.find('</pre>')>=0:
   if grammar:
    addProduction(name,choices,oneof)
   else:
    # dummy parse line for the sake of <i>/<em>
    a,b=parseLine(line.split('<pre>')[1])
   grammar = not grammar
   continue
  if grammar:
   cont = ifContinuation(line,oldline)
   oldline = line
   line = preprocess(cleanup(line))
   #print 'Parsing "'+line+'"...'
   a,b=parseLine(line)
   if a:
   # non-empty line
    if len(a)==2 and a[-1]==':':
     # new definition
     if choices:
      # flush the current one
      addProduction(name,choices,oneof)
     choices = []
     name = a[0]
     oneof = False
    elif len(a)==4 and a[0]==a[2] and a[1]==':' and a[-1]==':':
     # new mingled definition
     if choices:
      # flush the current one
      addProduction(name,choices,oneof)
     choices = []
     name = a[0]
     oneof = False
     print name,'double-declared, fixed'
     pessimistic[2] += 1
    elif len(a)==4 and a[1]==':' and a[2]=='one' and a[3]=='of':
     # new "one-of" definition
     if choices:
      addProduction(name,choices,oneof)
     choices = []
     name = a[0]
     oneof = True
    elif cont and choices:
     # line continuation
     print 'Line continuation enforced while parsing',name
     pessimistic[2] += 1
     for i in range(0,len(a)):
      choices[-1][0].append(a[i])
      choices[-1][1].append(b[i])
    else:
     # add choice branch
     choices.append([a,b])
 src.close()
 if pessimistic[1]:
  print 'Skipped',pessimistic[1],'anchor-containing snippets'

def printGrammarText(fn):
 ext = open(fn,'w')
 for nt in prods.keys():
  ext.write(serialiseT(nt,prods[nt]))
 ext.close()

def printGrammar(fn):
 ext = open(fn,'w')
 ext.write('<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
 for nt in prods.keys():
  ext.write(serialiseX(nt,prods[nt]))
 ext.write('</bgf:grammar>')
 ext.close()

def breakWords(nt,s):
 # transforms terminals like "aaa.bbb" to "aaa" "." "bbb"
 word = s[1:-1]
 res = '"'
 f = word[0].isalpha()
 for letter in word:
  if f==letter.isalpha():
   res += letter
  else:
   res += '" "'+letter
  f=letter.isalpha()
 cx = res.count(' ')
 if cx:
  print 'Multiple terminals heuristic fix:',s,'in',nt,'(1 to',`cx+1`+')'
  pessimistic[2] += 1
 return res+'"'

def automatedImprove():
 for nt in prods.keys():
  newprods = []
  for bs in prods[nt]:
   i=0
   while i<len(bs):
    if not bs[i]:
     i+=1
     continue
    if bs[i]=='?????':
     # Change to classic EBNF
     if i>0:
      newbs = bs[:i-1]
     else:
      newbs = []
     newbs.extend(['[',bs[i-1],']'])
     newbs.extend(bs[i+1:])
     bs = newbs
     continue
    if bs[i].rfind('opt')!=-1 and bs[i].rfind('opt')==len(bs[i])-3:
     print 'Structural heuristic fix:',bs[i],'in',nt,'(opt replaced by BNF optional)'
     pessimistic[2] += 1
     newbs = bs[:i]
     if bs[i]!='opt':
      newbs.append(bs[i][:-3])
     newbs.append('?????')
     newbs.extend(bs[i+1:])
     bs = newbs
     continue
    if bs[i]=='|' and nt.find('OrExpression')>=0:
     print 'Nonterminal to terminal heuristic fix:',bs[i],'in',nt,'(appropriate context)'
     pessimistic[2] += 1
     bs[i]='"|"'
     i+=1
     continue
    if bs[i]=='|' and i>1 and i+1<len(bs) and bs[i-2]=='"("' and bs[i+2]=='")"':
     # solution not generalised - brutal!
     # general problem here is when BNF bar is used without groups - it makes the extractor produce n((|)) instead of real choices
     newbs = bs[:i-1]
     newbs.append('(')
     newbs.extend(bs[i-1:i+2])
     newbs.append(')')
     newbs.extend(bs[i+2:])
     bs = newbs
     i -= 2
     pessimistic[2] += 1
     print 'Structural heuristic fix in',nt,'(group introduced)'
     continue
    if bs[i]=='"|"' and len(bs)>1 and nt.find('OrExpression')<0:
     print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(suspicious context)'
     pessimistic[2] += 1
     bs[i] = '|'
     continue
    if bs[i]!='.' and bs[i]!='"."' and bs[i]!='...' and bs[i]!='"..."' and bs[i].find('.')>=0:
     if bs[i][0]=='"':
      quote = True
      word = bs[i][1:-1]
     else:
      quote = False
      word = bs[i]
     if word[0]=='.' or word[-1]=='.':
      print 'Multiple terminals heuristic fix:',bs[i],'in',nt,'(1 to 2)'
     else:
      print 'Multiple terminals heuristic fix:',bs[i],'in',nt,'(1 to 3)'
     pessimistic[2] += 1
     if i>0:
      newbs = bs[:i-1]
     else:
      newbs = []
     if quote:
      if word[0]!='.':
       newbs.append('"'+word[:word.index('.')]+'"')
      newbs.append('"."')
      if word[-1]!='.':
       newbs.append('"'+word[word.index('.')+1:]+'"')
     else:
      if word[0]!='.':
       newbs.append(word[:word.index('.')])
      newbs.append('"."')
      if word[-1]!='.':
       newbs.append(word[word.index('.')+1:])
     if i+1<len(bs):
      newbs.extend(bs[i+1:])
     bs = newbs
     continue
    if bs[i][0]=='"':
     if bs[i][1].isupper() and bs[i][1:-1] in prods.keys():
      print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(familiar name)'
      pessimistic[2] += 1
      bs[i]=bs[i][1:-1]
      i+=1
      continue
     if bs[i]=='"opt"':
      print 'Structural heuristic fix:',bs[i],'in',nt,'(changed to BNF optional)'
      pessimistic[2] += 1
      bs[i]='?????'
      continue
     if bs[i].find('&')<0:
      bs[i] = breakWords(nt,bs[i])
     i+=1
     continue
    if bs[i].isalnum():
     if bs[i][0].islower() and (bs[i] not in prods.keys()):
      print 'Nonterminal to terminal heuristic fix:',bs[i],'in',nt,'(no definition)'
      pessimistic[2] += 1
      bs[i] = '"'+bs[i]+'"'
      continue
    elif bs[i] not in ('[',']','{','}','|','(',')'):
     print 'Nonterminal to terminal heuristic fix:',bs[i],'in',nt,'(weird name)'
     pessimistic[2] += 1
     bs[i] = '"'+bs[i]+'"'
     i+=1
     continue
    elif bs[i]==')':
     if '(' not in bs:
      # bracketing problem
      i+=1
      continue
     left = bs.index('(')
     if i+1<len(bs) and bs[i+1]=='?????':
      i+=1
      continue
     if '|' not in bs[left:i]:
      bs[left]='"("'
      bs[i]='")"'
      print 'Structural heuristic fix in',nt,'(useless group)'
      pessimistic[2] += 1
      i+=1
      continue
    i+=1
   newprods.append(fixBrackets(nt,' '.join(bs).split()))
  prods[nt]=newprods
 pass

def glueSymbols():
 for nt in prods.keys():
  newprods = []
  for bs in prods[nt]:
   for i in range(0,len(bs)-1):
    if not bs[i]:
     continue
    if bs[i][0]=='"' and len(bs[i])==3 and bs[i][1].isalpha():
     if bs[i+1][0]=='"':
      # "N" "ame"
      test = bs[i][1]+bs[i+1][1:-1]
     else:
      # "N" ame
      test = bs[i][1]+bs[i+1]
     if test.isalnum():
      if test in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       bs[i] = test
       bs[i+1]=''
       print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(familiar name)'
       pessimistic[2] += 2
      elif not (bs[i+1][0].isupper() or bs[i+1] in prods.keys()):
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       bs[i] = '"'+test+'"'
       bs[i+1]=''
       pessimistic[2] += 1
    elif bs[i][0]!='"' and len(bs[i])==1 and bs[i][0].isalpha():
     if bs[i+1][0]=='"':
      # N "ame"
      test = bs[i][0]+bs[i+1][1:-1]
     else:
      # N ame
      test = bs[i][0]+bs[i+1]
     if test.isalnum():
      if test in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       bs[i] = test
       bs[i+1]=''
       print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(familiar name)'
       pessimistic[2] += 2
      elif not (bs[i+1][0].isupper() or bs[i+1] in prods.keys()):
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       pessimistic[2] += 1
       bs[i] = '"'+test+'"'
       bs[i+1]=''
   for i in range(1,len(bs)):
    if not bs[i]:
     continue
    if bs[i][0]=='"' and len(bs[i])==3 and bs[i][1].isalpha():
     if bs[i-1][0]=='"':
      # "continu" "e"
      test = bs[i-1][1:-1]+bs[i][1]
     else:
      # continu "e"
      test = bs[i-1]+bs[i][1]
     if test.isalnum():
      if test in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       bs[i] = test
       bs[i-1]=''
       print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(familiar name)'
       pessimistic[2] += 2
      elif bs[i-1] not in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       pessimistic[2] += 1
       bs[i]='"'+test+'"'
       bs[i-1]=''
    elif bs[i][0]!='"' and len(bs[i])==1 and bs[i][0].isalpha():
     if bs[i-1][0]=='"':
      # "continu" e
      test = bs[i-1][1:-1]+bs[i][0]
     else:
      # continu e
      test = bs[i-1]+bs[i][0]
     if test.isalnum():
      if test in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       bs[i] = test
       bs[i-1]=''
       print 'Terminal to nonterminal heuristic fix:',bs[i],'in',nt,'(familiar name)'
       pessimistic[2] += 2
      elif bs[i-1] not in prods.keys():
       print 'Multiple terminals heuristic fix:','"'+test+'"','in',nt,'(2 to 1)'
       pessimistic[2] += 1
       bs[i]='"'+test+'"'
       bs[i-1]=''
   newprods.append(' '.join(bs).split())
  prods[nt]=newprods
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
  print 'Structural  heuristic fix in',nt,
  pessimistic[2] += 1
  #print arr,'->'
  arr.reverse()
  while(cx>0):
   if '"'+right+'"' in arr:
    arr[arr.index('"'+right+'"')]=right
    print '(transformed terminal)'
   elif left in arr:
    arr.remove(left)
    print '(removed left bracket)'
   else:
    print '(added right bracket)'
    arr=[right].extend(arr)
   cx -= 1
  arr.reverse()
  while(cx<0):
   if '"'+left+'"' in arr:
    arr[arr.index('"'+left+'"')]=left
    print '(transformed terminal)'
   elif right in arr:
    arr.remove(right)
    print '(removed right bracket)'
   else:
    print '(added left bracket)'
    arr=[left].extend(arr)
   cx += 1
  #print arr
  return arr

if __name__ == "__main__":
 print 'HTML to Grammar automated extractor'
 if len(sys.argv)==3 or len(sys.argv)==4:
  print 'Reading the HTML document...'
  readGrammar(sys.argv[1])
  print 'Massaging the grammar...'
  glueSymbols()
  automatedImprove()
  print 'Writing the extracted grammar...'
  if sys.argv[-1]=='-bnf':
   printGrammarText(sys.argv[2])
  else:
   printGrammar(sys.argv[2])
  if pessimistic[2]:
   print 'Total of',pessimistic[2]+pessimistic[1],'problems encountered and coped with.'
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'''<input> <output> [<options>]

Possible options:
	-bnf			Outputs in EBNF rather then in BGF'''
  sys.exit(1)

