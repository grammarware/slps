#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import sys
import glob
from elementtree import ElementTree
from lciConfig import *

# A global flag, if set, LCI will exit with a non-zero status
problem = False
# output streams redirected to null
shutup = ' 1> /dev/null 2> /dev/null'
graphBig = []
graphSmall = []
log = None
almostFailed = []
failed = []

def writeLog(s):
 log.write(s+'\n')
 log.flush()

def sysexit(n):
 log.close()
 sys.exit(n)

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

def expandxml(mixed,rep):
 s = mixed.text
 for tag in mixed.getchildren():
  s += expandone(tag.tag,tag.text,rep)
  s += tag.tail
 return s.strip()

def quote(a):
 return '"'+a+'"'

def makeGraph():
 # first we generate a complete picture
 for x in targets.keys():
  for branch in targets[x]:
   graphSmall.append((branch[0],x))
   if len(branch)==1:
    graphBig.append((branch,Chain(x),'',x,''))
   else:
    for i in range(1,len(branch)-1):
     if branch[i] in autoactions.keys():
      gen = autoactions[branch[i]]
     else:
      gen = ''
     graphBig.append((branch[:i],branch[:(i+1)],stripSpecifics(branch[i]),x,gen))
    if branch[-1] in autoactions.keys():
     gen = autoactions[branch[-1]]
    else:
     gen = ''
    graphBig.append((branch[:-1],Chain(x),stripSpecifics(branch[-1]),x,gen))

def distanceBetween(node,tgt):
 if node()==tgt:
  return '?'
 if len(targets[tgt])!=2:
  print '[WARN] Only binary branches supported for now.'
  return '??'
 if targets[tgt][0]().find(node())==0:
  return compareGrammars(node,targets[tgt][1])
 elif targets[tgt][1]().find(node())==0:
  return compareGrammars(node,targets[tgt][0])
 print '[FAIL]',node(),'not found on the way to target',tgt
 return '?'

def compareGrammars(bgf,arr):
 goal = arr[0]
 for a in arr().split('.')[1:]:
  if ttype[a] in ('preparation','nominal-matching','normalizing'):
   goal += '.'+stripCamelCase(a)
 #print '[----] Ready:',bgf,'vs',goal
 #print '[++++] Distance is:',
 #run = 'expr `'+tools['comparison'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep "Fail:" | wc -l` + `'+tools['comparison'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep "only:" | grep -o "\[..*\]" | wc -w`'
 run = tools['comparator'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep "only:" | grep -o "\[..*\]" | wc -w'
 writeLog(run)
 if os.system(run+' > TMP-res'):
  #print '[WARN] Cannot count name mismatches between',bgf(),'and',goal
  #return '?'
  nameDiffs = '0'
 else:
  num = open('TMP-res','r')
  nameDiffs = num.readline().strip()
  num.close()
 run = tools['comparator'] + ' bgf/'+bgf.bgfFileName()+' bgf/'+goal+'.bgf | grep Fail'
 writeLog(run)
 if os.system(run+' > TMP-res'):
  #print '[WARN] Cannot count structural mismatches between',bgf(),'and',goal
  #return '?'
  strDiffs = 0
 else:
  num = open('TMP-res','r')
  strDiffs = 0
  for line in num.readlines():
   nsn = line.strip().split('(')[1].split(')')[0].split('/')
   strDiffs += max(int(nsn[0]),int(nsn[1]))
  num.close()
 return nameDiffs+'+'+`strDiffs`

def dumpGraph(df):
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
  elif x in almostFailed:
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
  dot.write(quote(x+'_'+x)+' [label="'+x+'"]')
  if x in failed:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('node [shape=circle, style=solid];\n')
 # connect real target&source nodes with the "zero step" ones
 for x in targets.keys():
  for branch in targets[x]:
   if branch[0] in targets.keys():
    dot.write(quote(branch[0]+'_'+branch[0])+'->'+branch[:1].dotNodeName(x)+';')
   else:
    dot.write(quote(branch[0])+'->'+branch[:1].dotNodeName(x)+';')
 # continue
 nodezz=[]
 dablNodezz=[]
 for arc in graphBig:
  dot.write(arc[0].dotNodeName(arc[3]))
  dot.write('->'+arc[1].dotNodeName(arc[3]))
  if (arc[0],arc[3]) not in nodezz:
   nodezz.append((arc[0],arc[3]))
  if (arc[1],arc[3]) not in nodezz:
   nodezz.append((arc[1],arc[3]))
  if (arc[0][-1] in targets.keys()) or (arc[0][-1] in orderedsrc) or (arc[0][-1] in ttype.keys() and ttype[arc[0][-1]] in ('preparation','nominal-matching','normalizing')):
   if (arc[1][-1] in targets.keys()) or (arc[1][-1] in ttype.keys() and ttype[arc[1][-1]] in ('structural-matching','extension','correction','relaxation')):
    dablNodezz.append(arc[0].dotNodeName(arc[3]))
  par = ''
  if arc[2]:
   par += 'label="'+arc[2]+'" '
  if arc[1] in failed:
   par += 'color=red '
  if arc[4]:
   par += 'taillabel="'+arc[4]+'" labelfontname="Times-Italic" style=bold '
  if arc[1]() in targets.keys():
   pseudo = arc[0][:]
   pseudo.append(arc[2])
   if pseudo in failed:
    par += 'color=red '
  if par:
   dot.write(' ['+par+']')
  dot.write(';\n')
 #currentFileName = df+'.'+nodezz[0][1]+'.csv'
 #currentFile = open(currentFileName,'w')
 #print nodezz
 for nNg in nodezz:
  node,goal=nNg
  #if df+'.'+goal+'.csv' != currentFileName:
  # currentFile.close()
  # currentFileName = df+'.'+goal+'.csv'
  # currentFile = open(currentFileName,'w')
  if node in failed:
   colour = 'red'
  elif node in almostFailed:
   colour = 'blue'
  else:
   colour = 'black'
  if node() in targets.keys():
   dot.write(quote(node()+'_'+goal))
   tmp =quote(node()+'_'+goal)
   # test for end targets that do not need extra boxes
  else:
   dot.write(node.dotNodeName(goal))
   tmp = node.dotNodeName(goal)
  cx = distanceBetween(node,goal)
  #print '[----]',node,'(->',goal,') =',cx,'['+tmp+']'
  dot.write(' [color='+colour)
  if cx == '??':
   dot.write(', label="?"')
  elif cx != '?':
   dot.write(', label="'+cx+'"')
   #currentFile.write(node.lastAction()+','+`eval(cx)`+'\n')
  if node.dotNodeName(goal) in dablNodezz:
   # synch point
   dot.write(', shape=doublecircle')
  dot.write('];\n')
 #currentFile.close()
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_large.pdf 2>/dev/null'
 writeLog(run)
 if os.system(run):
  print '[WARN] Detailed diagram not generated'
  problem = True
 dot = open(df+'_small.dot','w')
 dot.write('digraph generated{ {rank=same; edge[style=invis,weight=10];\n')
 for x in orderedsrc:
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  elif x in almostFailed:
   dot.write(' [color=blue]')
  if x in derived.keys():
   dot.write(' [shape=egg]')
  dot.write(';')
 for x in orderedsrc:
  dot.write(quote(x))
  if x==orderedsrc[-1]:
   dot.write(';')
  else:
   dot.write('->')
 # "xsd"->"jaxb" [style=solid,label="xjc",color=grey,labelfloat,fontname="Times-Italic"];
 for x in derived.keys():
  dot.write('"'+derived[x][0]+'"->"'+x+'" [style=solid,label="'+derived[x][1]+'",color=grey,labelfloat,fontname="Times-Italic"];')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failed:
   dot.write(' [color=red]')
  elif x in almostFailed:
   dot.write(' [color=blue]')
  dot.write(';')
 for arc in graphSmall:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
  if arc[0] in failed and arc[1] in failed:
   dot.write(' [color=red]')
  dot.write(';\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_small.pdf 2>/dev/null'
 writeLog(run)
 if os.system(run):
  print '[WARN] Abstract diagram not generated.'
  problem = True
 else:
  print '[PASS] Diagram generation completed.'

def copyFile(x,y):
 try:
  xh=open(x,'r')
  yh=open(y,'w')
  yh.writelines(xh.readlines())
  xh.close()
  yh.close()
 except IOError,e:
  print '[FAIL] I/O error: either',x,'or',y,'is not accessible.'

def extractAll():
 for bgf in extractor.keys():
  run = extractor[bgf]+' bgf/'+bgf+'.bgf'
  writeLog(run)
  if os.system(run+shutup):
   if os.access('snapshot/'+bgf+'.bgf',os.R_OK):
    print '[WARN] Extraction of',bgf+'.bgf failed, LCI rolled back'
    copyFile('snapshot/'+bgf+'.bgf','bgf/'+bgf+'.bgf')
    writeLog('cp snapshot/'+bgf+'.bgf bgf/'+bgf+'.bgf')
    almostFailed.append(bgf)
   else:
    print '[FAIL] Extraction of',bgf+'.bgf failed'
    failed.append(Chain(bgf))
    problem = True
   #sysexit(3)
  else:
   run = tools['comparator'] + ' bgf/'+bgf+'.bgf snapshot/'+bgf+'.bgf'
   writeLog(run)
   if os.system(run+shutup):
    # different from the saved version
    print '[PASS] Extracted a newer version of',bgf+'.bgf'
    copyFile('bgf/'+bgf+'.bgf','snapshot/'+bgf+'.bgf')
    writeLog('cp bgf/'+bgf+'.bgf snapshot/'+bgf+'.bgf')
 print '[PASS] Extraction finished.'

def validateAll():
 problem = False
 for bgf in extractor.keys():
  if Chain(bgf) in failed:
   continue
  run = tools['validator']+' bgf/'+bgf+'.bgf'
  writeLog(run)
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
    run = automethods[autoactions[a]]+' bgf/'+current.bgfFileName()+' '+tools['extension']+'/'+a+'.'+tools['extension']
    writeLog(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
     ontheroll = False
    else:
     print '[PASS]',
    print 'Generated',ttype[a],a+'.'+tools['extension'],'from',current.bgfFileName()
    if ontheroll:
     run = tools['transformer']+' '+tools['extension']+'/'+a+'.'+tools['extension']+' bgf/'+current.bgfFileName()+' bgf/'+current.futureBgfFileName(a)
     writeLog(run)
     if os.system(run+shutup):
      problem = True
      print '[FAIL]',
      failed.append(current[:])
      failed[-1].append(a)
      ontheroll = False
     else:
      print '[PASS]',
     print 'Applied generated',a+'.'+tools['extension'],'to',current.bgfFileName()
   else:
    #??? 
    run = tools['transformer']+' '+tools['extension']+'/'+a+'.'+tools['extension']+' bgf/'+current.bgfFileName()+' bgf/'+current.futureBgfFileName(a)
    writeLog(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
     failed.append(current[:])
     failed[-1].append(a)
     ontheroll = False
    else:
     print '[PASS]',
    print 'Applied',ttype[a].replace('-',' '),a+'.'+tools['extension'],'to',current.bgfFileName()
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
 ontheroll,current = runTransforms(cut,current,('preparation','nominal-matching','normalizing'))
 if ontheroll:
  print '[PASS]',
 else:
  print '[FAIL]',
 print 'Postextraction and synchronization finished for target',target+'.'
 # same for transformation
 ontheroll,current = runTransforms(cut,current,('structural-matching','extension','correction','relaxation'))
 # end of branch
 if cut in failed:
  print '[FAIL]',
 else:
  print '[PASS]',
 print 'Branch finished as',current.bgfFileName()
 if cut not in failed and tools.has_key('validator'):
  a = tools['validator']+' bgf/'+current.bgfFileName()
  writeLog(a)
  if os.system(a+shutup):
   problem = True
   print '[FAIL]',
  else:
   print '[PASS]',
  print 'Branch result validated'
 return current

def orderTargets():
 unordered = targets.keys()[:]
 ordered = []
 while len(unordered):
  for t in unordered:
   flag = True
   for i in targets[t]:
    if (i[0] not in ordered) and (i[0] not in extractor.keys()):
     flag = False
   if flag:
    ordered.append(t)
    unordered.remove(t)
 return ordered

def buildTargets():
 for t in orderTargets():
  inputs = targets[t]
  fileinputs = []
  for i in range(0,len(inputs)):
   fileinputs.append(transformationChain(inputs[i],t))
  if len(inputs)>1:
   # need to diff
   diffAll(t,fileinputs[0],fileinputs[1:])
  # save resulting name
  cx = 0
  while cx<len(fileinputs):
   if fileinputs[cx] not in failed:
    break
   cx+=1
  if cx<len(fileinputs):
   print '[PASS] Target',t,'reached as',fileinputs[cx].bgfFileName()
   copyFile('bgf/'+fileinputs[cx].bgfFileName(),'bgf/'+t+'.bgf')
   writeLog('cp bgf/'+fileinputs[cx].bgfFileName()+' bgf/'+t+'.bgf')
  else:
   # Tough luck: all branches failed
   print '[FAIL] Target',t,'unreachable'

def diffAll(t,car,cdr):
 #print car,cdr
 if len(cdr)==1:
  run = tools['comparator']+' bgf/'+car.bgfFileName()+' bgf/'+cdr[0].bgfFileName()
  writeLog(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Mismatch in target',t+':',car.bgfFileName(),'differs from',cdr[0].bgfFileName()
   failed.append(Chain(t))
   #sysexit(3)
 else:
  for head in cdr:
   diffAll(t,car,[head])
  diffAll(t,cdr[0],cdr[1:])

def chainXBTF(testcase,steps,t):
 re = fr = testcase
 for step in steps:
  if step==steps[-1]:
   # name it after the target
   re = fr.split('.')[0]+'.'+t+'.btf'
  else:
   # name it as input.transformationName.btf
   re = '.'.join(fr.split('.')[:-1])+'.'+step+'.btf'
  run = treeTools['transformer']+' '+tools['extension']+'/'+step+'.'+tools['extension']+' '+fr+' '+re
  writeLog(run)
  #print 'Performing coupled',step,'on',fr,'-',
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Performing coupled',step,'on',fr,'failed'
   break
  fr = re
 print '[PASS] Performed coupled',steps.spaceNotation(),'on',testcase,
 if treeTools.has_key('validator'):
  run = treeTools['validator']+' '+re
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
 if 'comparator' not in treeTools.keys():
  # no tree diff tool specified
  return
 basetestset = testsets.keys()[0]
 for basetestcase in glob.glob(basetestset+'/*.'+t+'.btf'):
  for testset in testsets.keys()[1:]:
   for testcase in glob.glob(testset+'/'+basetestcase.split('/')[1]):
    run = treeTools['comparator']+' '+basetestcase+' '+testcase
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
    else:
     print '[PASS]',
    print 'Found and compared',basetestcase.split('/')[1],'in',basetestset,'and',testset

def convergeTestSets():
 for testset in testsets.keys():
  # extracting
  run = testsets[testset]+' '+testset
  writeLog(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Test set',testset,'could not be extracted'
   continue
  print '[PASS] Test set',testset,'extracted'
 for src in treeExtractor.keys():
  for testset in tester[src]:
   for testcase in glob.glob(testset+'/*.src'):
    run = treeExtractor[src]+' '+testcase+' '+testcase+'.btf'
    writeLog(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
    else:
     print '[PASS]',
    print 'Tree extracted from',testcase
 for t in orderTargets():
  for branch in targets[t]:
   if treeExtractor.has_key(branch[0]):
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
 final = orderTargets()[-1]
 for evaluator in treeEvaluator.keys():
  #wtf?
  pass

def runTestSets():
 for testset in testsets.keys():
  # testing parser
  for testcase in glob.glob(testset+'/*.src'):
   results={}
   for program in parser.keys():
    if testset in tester[program]:
     run = parser[program]+' '+testcase
     writeLog(run)
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
     writeLog(run)
     results[program]=os.system(run+shutup)
   if results.values()==[0]*len(results):
    print '[PASS] Test case',testcase,'evaluated'
   else:
    problem = True
    print '[FAIL] Test case',testcase,'failed evaluation'
    for r in results.keys():
     if results[r]:
      print '[FAIL]',r,'evaluated it differently'

def checkConsistency():
 # some simple assertions
 # all targets depend on existing targets or sources
 for t in targets.keys():
  for i in targets[t]:
   if not (targets.has_key(i[0]) or extractor.has_key(i[0])):
    print '[FAIL] Target',t,'needs',i[0],'which is not defined'
    sysexit(7)
 # all actions can be found
 try:
  for a in actions:
   if a not in autoactions.keys():
    open(tools['extension']+'/'+a+'.'+tools['extension'],'r').close()
 except IOError, e:
  print '[FAIL] Undefined action used: need',e.filename
  #sysexit(8)
 # all automated actions can be found
 for a in autoactions.keys():
  if autoactions[a] not in automethods.keys():
   print '[FAIL] Automation method',autoactions[a],'not found (automated action',a+')'
   sysexit(18)

if __name__ == "__main__":
 print 'Language Covergence Infrastructure v1.18'
 if len(sys.argv) == 3:
  log = open(sys.argv[1].split('.')[0]+'.log','w')
  readConfiguration(sys.argv[1])
  if 'extension' not in tools.keys():
   tools['extension'] = 'xbgf'
  checkConsistency()
  makeGraph()
  extractAll()
  if tools.has_key('validator'):
   validateAll()
  buildTargets()
  print '----- Grammar convergence phase finished. -----'
  if testsets:
   runTestSets()
   convergeTestSets()
   print '----- Tree convergence phase finished. -----'
  else:
   print '[WARN] No testing performed.'
  # DEBUG - no diagram
  dumpGraph(sys.argv[2])
  # print '[WARN] Diagram generation disabled.'
  if problem:
   sysexit(100)
  log.close()
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration file>','<diagram prefix>'
  sys.exit(1)

