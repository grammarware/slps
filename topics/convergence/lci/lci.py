#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import sys
import glob
from elementtree import ElementTree

class Chain:
 def __init__(self,*arr):
  self.array=[]
  for a in arr:
   self.array.append(a)
 def __call__(self):
  return '.'.join(self.array)
 def __repr__(self):
  return '-'+'.'.join(self.array)+'-'
 # singular items are strings, slices are Chains
 def __getitem__(self,key):
  if type(key)==type(slice(0,1,None)):
   return ChainFromArray(self.array[key])
  else:
   return self.array[key]
 def dotNodeName(self):
  name = self.array[0]
  for a in self.array[1:]:
   name += '_'+stripSelector1(a)
  return name
 def bgfFileName(self):
  name = self.array[0]
  for a in self.array[1:]:
   name += '.'+stripSelector2(a)
  return name+'.bgf'
 def futureBgfFileName(self,next):
  name = self.array[0]
  for a in self.array[1:]:
   name += '.'+stripSelector2(a)
  return name+'.'+stripSelector2(next)+'.bgf'
 def append(self,step):
  self.array.append(step)
 def __len__(self):
  return len(self.array)
 def spaceNotation(self):
  tmp = self.array[:]
  tmp.reverse()
  return ' '.join(tmp)
 def __eq__(self,other):
  if type(other)==type(''):
   return self.array==other.split('.')
  else:
   return self.array==other.array

def ChainFromArray(a):
 x = Chain()
 x.array = a[:]
 return x

# A global flag, if set, LCI will exit with a non-zero status
problem = False
# output streams redirected to null
shutup = ' 1> /dev/null 2> /dev/null'
# transformation type per action: postextraction, synchronization, etc
ttype = {}
orderedsrc = []
shortcuts = {}
actions = []
autoactions = {}
testsets = {}
tester = {}
extractor = {}
treeextractor = {}
treeevaluator = {}
targets = {}
parser = {}
evaluator = {}
testset = []
graph_big = []
graph_small = []
log = None
tools = {}
treetools = {}
automethods = {}
almostfailed = []
failed = []

def logwrite(s):
 log.write(s+'\n')
 log.flush()

def sysexit(n):
 log.close()
 sys.exit(n)

def readxmlconfig (cfg):
 config = ElementTree.parse(cfg)
 # shortcuts
 for xmlnode in config.findall('//shortcut'):
  shortcuts[xmlnode.findtext('name')]=expandxml(xmlnode.findall('expansion')[0],{})
 # actions
 for xmlnode in config.findall('//target/branch/*/perform'):
  if xmlnode.text not in actions:
   actions.append(xmlnode.text)
 # automated actions
 for xmlnode in config.findall('//target/branch/*/automated'):
  if xmlnode.findtext('result') not in actions:
   actions.append(xmlnode.findtext('result'))
   autoactions[xmlnode.findtext('result')]=xmlnode.findtext('method')
 # testset
 for xmlnode in config.findall('//testset'):
  testsets[xmlnode.findtext('name')]=expandxml(xmlnode.findall('command')[0],{})
 # sources
 for xmlnode in config.findall('//source'):
  orderedsrc.append(xmlnode.findtext('name'));
  extractor[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/extraction')[0],{})
  if xmlnode.findall('grammar/parsing'):
   parser[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/parsing')[0],{})
  if xmlnode.findall('grammar/evaluation'):
   evaluator[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/evaluation')[0],{})
  if xmlnode.findall('tree/extraction'):
   treeextractor[xmlnode.findtext('name')]=expandxml(xmlnode.findall('tree/extraction')[0],{})
  if xmlnode.findall('tree/evaluation'):
   treeevaluator[xmlnode.findtext('name')]=expandxml(xmlnode.findall('tree/evaluation')[0],{})
  tmp = []
  for set in xmlnode.findall('testing/set'):
   tmp.append(set.text)
  tester[xmlnode.findtext('name')]=tmp[:]
 # targets
 for xmlnode in config.findall('//target'):
  name = xmlnode.findtext('name')
  targets[name]= [[],'']
  for br in xmlnode.findall('branch'):
   for phase in br.findall('*'):
    if phase.tag == 'input':
     branch = Chain(br.findtext('input'))
    else:
     for p in phase.findall('*'):
      if p.tag == 'perform':
       branch.append(p.text)
       ttype[p.text] = phase.tag
      elif p.tag == 'automated':
       branch.append(p.findtext('result'))
       ttype[p.findtext('result')] = phase.tag
      else:
       print '[WARN] Unknown tag skipped:',p.tag
   targets[name][0].append(branch)
 # tools
 for xmlnode in config.findall('//tool'):
  tools[xmlnode.findtext('name')] = expandxml(xmlnode.findall('grammar')[0],{})
  if xmlnode.findall('tree'):
   treetools[xmlnode.findtext('name')] = expandxml(xmlnode.findall('tree')[0],{})
 # methods
 for xmlnode in config.findall('//generator'):
  automethods[xmlnode.findtext('name')] = expandxml(xmlnode.findall('command')[0],{})
 print 'Read',
 if shortcuts:
  print len(shortcuts),'shortcuts,',
 if tools or treetools:
  print `len(tools)`+'+'+`len(treetools)`,'tools,',
 if actions:
  if autoactions:
   print len(actions),'actions ('+`len(autoactions)`,'automated),',
  else:
   print len(actions),'actions,',
 if automethods:
  print len(automethods),'generators,',
 if targets:
  print len(targets),'targets,',
 if testsets:
  print len(testsets),'test sets,',
 if extractor:
  print len(extractor),'sources,',
 if parser or evaluator:
  print len(parser),'parsers &',len(evaluator),'evaluators,',
 print 'LCF is fine.'

def expandone(tag,text,rep):
 if text:
  wte = text
 else:
  wte = tag.replace('expand-','')
 if shortcuts.has_key(wte):
  return shortcuts[wte]
 elif rep.has_key(wte):
  return rep[wte]
 else:
  # postpone expanding
  return '%'+wte+'%'

def expandxml (mixed,rep):
 s = mixed.text
 for tag in mixed.getchildren():
  s += expandone(tag.tag,tag.text,rep)
  s += tag.tail
 return s.strip()

def expanduni(where,rep):
 cut = where.split('%')
 for i in range(0,len(cut)):
  if i%2:
   if shortcuts.has_key(cut[i]):
    cut[i]=shortcuts[cut[i]]
   elif rep.has_key(cut[i]):
    cut[i]=rep[cut[i]]
   else:
    print '[FAIL] Misused expand, referencing undefined "'+cut[i]+'":'
    sysexit(11)
 return ''.join(cut)

def quote(a):
 return '"'+a+'"'

def stripSelector1(lbl):
 l = lbl[:]
 if l.find('-')>0:
  l = l.split('-')[0]
 return l

def stripSelector2(lbl):
 l=''
 for x in lbl:
  if x.islower() or x=='.':
   l+=x
  else:
   break
 return l

def addarc(fromnode,tonode,q,labelnode):
 if [fromnode,tonode,q,labelnode] not in graph_big:
  graph_big.append([fromnode,tonode,q,labelnode])

def makegraph():
 # first we generate a complete picture
 for x in targets.keys():
  for branch in targets[x][0]:
   graph_small.append((branch[0],x))
   if len(branch)==1:
    graph_big.append((branch,Chain(x),''))
   else:
    for i in range(1,len(branch)-1):
     graph_big.append((branch[:i],branch[:(i+1)],stripSelector1(branch[i])))
    graph_big.append((branch[:-1],Chain(x),stripSelector1(branch[-1])))

def distanceFrom(node):
 for t in targets.keys():
  if len(targets[t][0])!=2:
   print '[WARN] Only binary branches supported for now.'
   continue
  if targets[t][0][0]().find(node())==0:
   return compareGrammars(node,targets[t][0][1])
  elif targets[t][0][1]().find(node())==0:
   return compareGrammars(node,targets[t][0][0])
 print '[FAIL]',node(),'not found in',targets
 return '?'

def compareGrammars(bgf,arr):
 goal = arr[0]
 for a in arr().split('.')[1:]:
  if ttype[a] in ('synchronization','postextraction'):
   goal += '.'+stripSelector2(a)
 #print '[----] Ready:',bgf,'vs',goal
 #print '[++++] Distance is:',
 run = 'expr `'+tools['comparison'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep "Fail:" | wc -l` + `'+tools['comparison'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep "only:" | grep -o "\[..*\]" | wc -w`'
 logwrite(run)
 if os.system(run+' > TMP-res'):
  print '[WARN] Cannot measure the distance between',bgf(),'and',goal
  return '?'
 num = open('TMP-res','r')
 n = num.readline().strip()
 num.close()
 return n

def dumpgraph(df):
 dot = open(df+'_large.dot','w')
 dot.write('''digraph generated{
 edge [fontsize=24];
 node [fontsize=24];
 {rank=same;
 node [shape=ellipse, style=bold];
 edge[style=invis,weight=10];
 ''')
 for x in orderedsrc:
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  dot.write(';')
 for x in orderedsrc:
  dot.write(quote(x))
  if x==orderedsrc[-1]:
   dot.write(';')
  else:
   dot.write('->')
 dot.write('}\n')
 dot.write('node [shape=octagon, style=bold];\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('node [shape=circle, style=solid];\n')
 nodezz=[]
 done = []
 for arc in graph_big:
  if (arc[0].dotNodeName(),arc[1].dotNodeName()) in done:
   continue
  dot.write(arc[0].dotNodeName()+'->'+arc[1].dotNodeName())
  done.append((arc[0].dotNodeName(),arc[1].dotNodeName()))
  if arc[0] not in nodezz and arc[0]() not in targets.keys() and arc[0]() not in extractor.keys():
   nodezz.append(arc[0])
  if arc[1] not in nodezz and arc[1]() not in targets.keys() and arc[1]() not in extractor.keys():
   nodezz.append(arc[1])
  par = ''
  if arc[2]:
   par += 'label="'+arc[2]+'" '
  if arc[1] in failed:
   par += 'color=red '
  if arc[1]() in targets.keys():
   pseudo = arc[0][:]
   pseudo.append(arc[2])
   if pseudo in failed:
    par += 'color=red '
  if par:
   dot.write(' ['+par+']')
  dot.write(';\n')
 for node in nodezz:
  if node in failed:
   colour = 'red'
  elif node in almostfailed:
   colour = 'blue'
  else:
   colour = 'black'
  # labels not needed anymore because nodes became points
  dot.write(node.dotNodeName()+' [color='+colour+', label="'+distanceFrom(node)+'"];\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_large.pdf'
 logwrite(run)
 if os.system(run):
  print '[WARN] Detailed diagram not generated'
  problem = True
 dot = open(df+'_small.dot','w')
 dot.write('digraph generated{ {rank=same; edge[style=invis,weight=10];\n')
 for x in orderedsrc:
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  dot.write(';')
 for x in orderedsrc:
  dot.write(quote(x))
  if x==orderedsrc[-1]:
   dot.write(';')
  else:
   dot.write('->')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  dot.write(';')
 for arc in graph_small:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
  if arc[0] in failed and arc[1] in failed:
   dot.write(' [color=red]')
  dot.write(';\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_small.pdf'
 logwrite(run)
 if os.system(run):
  print '[WARN] Abstract diagram not generated.'
  problem = True
 else:
  print '[PASS] Diagram generation completed.'

def copyfile(x,y):
 xh=open(x,'r')
 yh=open(y,'w')
 yh.writelines(xh.readlines())
 xh.close()
 yh.close()

def extractall():
 for bgf in extractor.keys():
  run = extractor[bgf]+' bgf/'+bgf+'.bgf'
  logwrite(run)
  if os.system(run+shutup):
   if os.access('snapshot/'+bgf+'.bgf',os.R_OK):
    print '[WARN] Extraction of',bgf+'.bgf failed, LCI rolled back'
    copyfile('snapshot/'+bgf+'.bgf','bgf/'+bgf+'.bgf')
    logwrite('cp snapshot/'+bgf+'.bgf bgf/'+bgf+'.bgf')
    almostfailed.append(bgf)
   else:
    print '[FAIL] Extraction of',bgf+'.bgf failed'
    failed.append(Chain(bgf))
    problem = True
   #sysexit(3)
  else:
   run = tools['comparison'] + ' bgf/'+bgf+'.bgf snapshot/'+bgf+'.bgf'
   logwrite(run)
   if os.system(run+shutup):
    # different from the saved version
    print '[PASS] Extracted a newer version of',bgf+'.bgf'
    copyfile('bgf/'+bgf+'.bgf','snapshot/'+bgf+'.bgf')
    logwrite('cp bgf/'+bgf+'.bgf snapshot/'+bgf+'.bgf')
 print '[PASS] Extraction finished.'

def validateall():
 problem = False
 for bgf in extractor.keys():
  if Chain(bgf) in failed:
   continue
  run = tools['validation']+' bgf/'+bgf+'.bgf'
  logwrite(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Validation failed on',bgf+'.bgf'
   failed.append(Chain(bgf))
   #sysexit(3)
 if not problem:
  print '[PASS] Validation finished.'

def runTransforms(cut,current,whichtypes):
 ontheroll = True
 for a in cut[1:]:
  if ontheroll:
   if ttype[a] not in whichtypes:
    continue
   if a in autoactions.keys():
    #print 'Automated action',a,'spotted!
    run = automethods[autoactions[a]]+' bgf/'+current.bgfFileName()+' xbgf/'+a+'.xbgf'
    logwrite(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
     ontheroll = False
    else:
     print '[PASS]',
    print 'Generated',ttype[a],a+'.xbgf','from',current.bgfFileName()
    if ontheroll:
     run = tools['transformation']+' xbgf/'+a+'.xbgf bgf/'+current.bgfFileName()+' bgf/'+current.futureBgfFileName(a)
     logwrite(run)
     if os.system(run+shutup):
      problem = True
      print '[FAIL]',
      failed.append(current[:])
      failed[-1].append(a)
      ontheroll = False
     else:
      print '[PASS]',
     print 'Applied generated',a+'.xbgf','to',current.bgfFileName()
   else:
    #??? 
    run = tools['transformation']+' xbgf/'+a+'.xbgf bgf/'+current.bgfFileName()+' bgf/'+current.futureBgfFileName(a)
    logwrite(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
     failed.append(current[:])
     failed[-1].append(a)
     ontheroll = False
    else:
     print '[PASS]',
    print 'Applied',ttype[a],a+'.xbgf','to',current.bgfFileName()
  else:
   failed.append(current[:])
   failed[-1].append(a)
  current.append(a)
 return ontheroll,current

def transformationChain(cut,target):
 # executes preparational actions (abstract, unerase, etc) before comparison
 if len(cut)==1:
  # nothing to do
  return Chain(cut[0])
 else:
  # start point at source or target
  current = Chain(cut[0])
 # action names will be appended:
 # x.bgf -> x.corrupt.bgf -> x.corrupt.confuse.bgf -> x.corrupt.confuse.destroy.bgf -> ...
 # the very last one will be diffed
 ontheroll,current = runTransforms(cut,current,('postextraction','synchronization','normalization'))
 if ontheroll:
  print '[PASS]',
 else:
  print '[FAIL]',
 print 'Postextraction and synchronyzation finished for target',target+'.'
 # same for transformation
 ontheroll,current = runTransforms(cut,current,('transformation','refactoring'))
 # end of branch
 if cut in failed:
  print '[FAIL]',
 else:
  print '[PASS]',
 print 'Branch finished as',current.bgfFileName()
 if cut not in failed and tools.has_key('validation'):
  a = tools['validation']+' bgf/'+current.bgfFileName()
  logwrite(a)
  if os.system(a+shutup):
   problem = True
   print '[FAIL]',
  else:
   print '[PASS]',
  print 'Branch result validated'
 return current

def ordertargets():
 unordered = targets.keys()[:]
 ordered = []
 while len(unordered):
  for t in unordered:
   flag = True
   for i in targets[t][0]:
    if (i[0] not in ordered) and (i[0] not in extractor.keys()):
     flag = False
   if flag:
    ordered.append(t)
    unordered.remove(t)
 return ordered

def buildtargets():
 for t in ordertargets():
  inputs = targets[t][0]
  fileinputs = []
  for i in range(0,len(inputs)):
   fileinputs.append(transformationChain(inputs[i],t))
  if len(inputs)>1:
   # need to diff
   diffall(t,fileinputs[0],fileinputs[1:])
  # save resulting name
  cx = 0
  while cx<len(fileinputs):
   if fileinputs[cx] not in failed:
    break
   cx+=1
  if cx<len(fileinputs):
   print '[PASS] Target',t,'reached as',fileinputs[cx].bgfFileName()
   copyfile('bgf/'+fileinputs[cx].bgfFileName(),'bgf/'+t+'.bgf')
   logwrite('cp bgf/'+fileinputs[cx].bgfFileName()+' bgf/'+t+'.bgf')
  else:
   # Tough luck: all branches failed
   print '[FAIL] Target',t,'unreachable'
  targets[t][1] = t

def diffall(t,car,cdr):
 #print car,cdr
 if len(cdr)==1:
  run = tools['comparison']+' bgf/'+car.bgfFileName()+' bgf/'+cdr[0].bgfFileName()
  logwrite(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Mismatch in target',t+':',car.bgfFileName(),'differs from',cdr[0].bgfFileName()
   failed.append(Chain(t))
   #sysexit(3)
 else:
  for head in cdr:
   diffall(t,car,[head])
  diffall(t,cdr[:1],cdr[1:])

def chainXBTF(testcase,steps,t):
 fr = testcase
 for step in steps:
  if step==steps[-1]:
   # name it after the target
   re = fr.split('.')[0]+'.'+t+'.btf'
  else:
   # name it as input.transformationName.btf
   re = '.'.join(fr.split('.')[:-1])+'.'+step+'.btf'
  run = treetools['transformation']+' xbgf/'+step+'.xbgf '+fr+' '+re
  logwrite(run)
  #print 'Performing coupled',step,'on',fr,'-',
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Performing coupled',step,'on',fr,'failed'
   break
  fr = re
 print '[PASS] Performed coupled',steps.spaceNotation(),'on',testcase,
 if treetools.has_key('validation'):
  run = treetools['validation']+' '+re
  if os.system(run+shutup):
   problem = True
   print '- NOT valid'
  else:
   print '- valid'
 else:
  print

def diffBTFs(t):
 if len(testsets)<2:
  # with one test set there's nothing to diff
  return
 if not treetools.has_key('comparison'):
  # no tree diff tool specified
  return
 basetestset = testsets.keys()[0]
 for basetestcase in glob.glob(basetestset+'/*.'+t+'.btf'):
  for testset in testsets.keys()[1:]:
   for testcase in glob.glob(testset+'/'+basetestcase.split('/')[1]):
    run = treetools['comparison']+' '+basetestcase+' '+testcase
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
    else:
     print '[PASS]',
    print 'Found and compared',basetestcase.split('/')[1],'in',basetestset,'and',testset

def convergetestset():
 for testset in testsets.keys():
  # extracting
  run = testsets[testset]+' '+testset
  logwrite(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Test set',testset,'could not be extracted'
   continue
  print '[PASS] Test set',testset,'extracted'
 for src in treeextractor.keys():
  for testset in tester[src]:
   for testcase in glob.glob(testset+'/*.src'):
    run = treeextractor[src]+' '+testcase+' '+testcase+'.btf'
    logwrite(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
    else:
     print '[PASS]',
    print 'Tree extracted from',testcase
 for t in ordertargets():
  for branch in targets[t][0]:
   if treeextractor.has_key(branch[0]):
    # it's a source, let's check it we have an extracted tree
    for testset in tester[branch[0]]:
     for testcase in glob.glob(testset+'/*.src.btf'):
      chainXBTF(testcase,branch[1:],t)
   if targets.has_key(branch[0]):
    # it's a target, let's see if we have any test cases arrived at it
    for testset in testsets.keys():
     for testcase in glob.glob(testset+'/*.'+branch[0]+'.btf'):
      chainXBTF(testcase,branch[1:],t)
  diffBTFs(t)
 final = ordertargets()[-1]
 for evaluator in treeevaluator.keys():
  pass

def runtestset():
 for testset in testsets.keys():
  # testing parser
  for testcase in glob.glob(testset+'/*.src'):
   results={}
   for program in parser.keys():
    if testset in tester[program]:
     run = parser[program]+' '+testcase
     logwrite(run)
     results[program]=os.system(run+shutup)
   if results.values()==[0]*len(results):
    print '[PASS] Test case',testcase,'parsed'
   else:
    problem = True
    print '[FAIL] Test case',testcase,'failed parsing'
    for r in results.keys():
     if results[r]:
      print '[FAIL]',r,'did not parse it correctly'
  # testing evaluator
  for testcase in glob.glob(testset+'/*.run'):
   results={}
   for program in evaluator.keys():
    if testset in tester[program]:
     run = evaluator[program]+' '+testcase.replace('.run','.ctx')+' '+testcase+' '+testcase.replace('.run','.val')
     logwrite(run)
     results[program]=os.system(run+shutup)
   if results.values()==[0]*len(results):
    print '[PASS] Test case',testcase,'evaluated'
   else:
    problem = True
    print '[FAIL] Test case',testcase,'failed evaluation'
    for r in results.keys():
     if results[r]:
      print '[FAIL]',r,'evaluated it differently'

def checkconsistency():
 # some simple assertions
 # all targets depend on existing targets or sources
 for t in targets.keys():
  for i in targets[t][0]:
   if not (targets.has_key(i[0]) or extractor.has_key(i[0])):
    print '[FAIL] Target',t,'needs',i[0],'which is not defined'
    sysexit(7)
 # all actions can be found
 try:
  for a in actions:
   if a not in autoactions.keys():
    open('xbgf/'+a+'.xbgf','r').close()
 except IOError, e:
  print '[FAIL] Undefined action used: need',e.filename
  #sysexit(8)
 # all automated actions can be found
 for a in autoactions.keys():
  if autoactions[a] not in automethods.keys():
   print '[FAIL] Automation method',autoactions[a],'not found (automated action',a+')'
   sysexit(18)

if __name__ == "__main__":
 print 'Language Covergence Infrastructure v1.15'
 if len(sys.argv) == 3:
  log = open(sys.argv[1].split('.')[0]+'.log','w')
  readxmlconfig(sys.argv[1])
  checkconsistency()
  makegraph()
  extractall()
  if tools.has_key('validation'):
   validateall()
  buildtargets()
  print '----- Grammar convergence phase finished. -----'
  if testsets:
   runtestset()
   convergetestset()
   print '----- Tree convergence phase finished. -----'
  else:
   print '[WARN] No testing performed.'
  dumpgraph(sys.argv[2])
  if problem:
   sysexit(100)
  log.close()
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration file>','<diagram prefix>'
  sysexit(1)
