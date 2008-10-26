#!/usr/bin/python
import os
import sys
import glob
from elementtree import ElementTree

# A global flag, if set, LCI will exit with a non-zero status
problem = False

shutup = ' 1> /dev/null 2> /dev/null'
orderedsrc = []
shortcuts = {}
actions = []
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
almostfailed = []
failednode = []
failedarc  = []
failedaction = []

def postfix2prefix(post):
 #  input: 'x.c.b.a'
 # output: 'a b c x'
 pre = post.split('.')
 pre.reverse()
 return ' '.join(pre)

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
 for xmlnode in config.findall('//target/branch/perform'):
  if xmlnode.text not in actions:
   actions.append(xmlnode.text)
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
   branch = [br.findtext('input')]
   for p in br.findall('perform'):
    branch.append(p.text)
   targets[name][0].append(branch)
 # tools
 for xmlnode in config.findall('//tool'):
  tools[xmlnode.findtext('name')] = expandxml(xmlnode.findall('grammar')[0],{})
  if xmlnode.findall('tree'):
   treetools[xmlnode.findtext('name')] = expandxml(xmlnode.findall('tree')[0],{})

 print 'Read',
 if shortcuts:
  print len(shortcuts),'shortcuts,',
 if tools or treetools:
  print `len(tools)`+'+'+`len(treetools)`,'tools,',
 if actions:
  print len(actions),'actions,',
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

def stripSelector(lbl):
 l = lbl[:]
 if l.find('-')>0:
  l = l.split('-')[0]
 return l

def addarc(fromnode,tonode,q,labelnode):
 if [fromnode,tonode,q,labelnode] not in graph_big:
  graph_big.append([fromnode,tonode,q,labelnode])

def makegraph():
 # first we generate a complete picture
 for x in targets.keys():
  for src in targets[x][0]:
   if len(src)==1:
    addarc(src[0],x,'','')
   else:
    name  = src[0]
    qname = src[0]
    for i in range(1,len(src)-1):
     qname += '_'+stripSelector(src[i])
     addarc(name,qname,name,stripSelector(src[i]))
     name = qname
    addarc(name,x,qname,stripSelector(src[-1]))
 # make a simplified one
 for x in targets.keys():
  for src in targets[x][0]:
   graph_small.append([src[0],x])

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
  if x in failednode:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  if x==orderedsrc[-1]:
   dot.write(';')
  else:
   dot.write('->')
 dot.write('}\n')
 dot.write('node [shape=octagon, style=bold];\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('node [shape=point, style=solid];\n')
 nodezz=[]
 for arc in graph_big:
  #dot.write(quote(arc[0])+'->'+quote(arc[1]))
  dot.write(arc[0]+'->'+arc[1])
  if arc[0] not in nodezz:
   nodezz.append(arc[0])
  if arc[1] not in nodezz:
   nodezz.append(arc[1])
  par = ''
  if arc[3]:
   par += 'label="'+arc[3]+'" '
  if [arc[2],arc[3]] in failedarc:
   par += 'color=red '
  if par:
   dot.write(' ['+par+']')
  dot.write(';\n')
 for node in nodezz:
  if node not in extractor.keys():
   if node not in targets.keys():
    if node in failednode:
     colour = 'red'
    elif node in almostfailed:
     colour = 'blue'
    else:
     colour = 'black'
    label = node.split('_')
    label = label[0]+("'"*(len(label)-1))
    dot.write(node+' [label="'+label+'" color='+colour+'];')
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
  if x in failednode:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  if x == orderedsrc[-1]:
   dot.write(';')
  else:
   dot.write('->')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
  elif x in almostfailed:
   dot.write(' [color=blue]')
  dot.write(';')
 for arc in graph_small:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
  if arc[0] in failednode and arc[1] in failednode:
   dot.write(' [color=red]')
  dot.write(';\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_small.pdf'
 logwrite(run)
 if os.system(run):
  print '[WARN] Abstract diagram not generated'
  problem = True

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
    failednode.append(bgf)
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
  if bgf in failednode:
   continue
  run = tools['validation']+' bgf/'+bgf+'.bgf'
  logwrite(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Validation failed on',bgf+'.bgf'
   failednode.append(bgf)
   #sysexit(3)
 if not problem:
  print '[PASS] Validation finished.'

def preparebgf(cut):
 # executes preparational actions (abstract, unerase, etc) before comparison
 if len(cut)==1:
  return cut[0]
 else:
  if cut[0] in extractor.keys():
   # starting point is a source
   curname = cut[0]
  else:
   # starting point is another target
   curname = targets[cut[0]][1]
  # action names will be appended:
  # x.bgf -> x.corrupt.bgf -> x.corrupt.confuse.bgf -> x.corrupt.confuse.destroy.bgf -> ...
  # the very last one will be diffed
  ontheroll = True
  for a in cut[1:]:
   if ontheroll:
    run = tools['transformation']+' xbgf/'+a+'.xbgf bgf/'+curname+'.bgf bgf/'+curname+'.'+stripSelector(a)+'.bgf'
    logwrite(run)
    if os.system(run+shutup):
     problem = True
     print '[FAIL]',
     failedarc.append([curname,a])
     failednode.append(cut[0]+"'"*(curname.count('.')+1))
     failedaction.append(postfix2prefix(curname+'.'+stripSelector(a)))
     ontheroll = False
    else:
     print '[PASS]',
    print 'Applied',a+'.xbgf','to',stripSelector(curname)+'.bgf'
   else:
    failedarc.append([curname,a])
    failednode.append(cut[0]+"'"*(curname.count('.')+1))
    failedaction.append(postfix2prefix(curname+'.'+stripSelector(a)))
   curname += '.'+stripSelector(a)
 name = postfix2prefix('.'.join(cut))
 if name in failedaction:
  print '[FAIL]',
 else:
  print '[PASS]',
 print 'Branch finished'
 if name not in failedaction and tools.has_key('validation'):
  a = tools['validation']+' bgf/'+curname+'.bgf'
  logwrite(a)
  if os.system(a+shutup):
   problem = True
   print '[FAIL]',
  else:
   print '[PASS]',
  print 'Branch result validated'
 return curname

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
  fileinputs = ['']*len(inputs)
  for i in range(0,len(inputs)):
   fileinputs[i] = preparebgf(inputs[i])
  if len(inputs)>1:
   # need to diff
   diffall(t,fileinputs[0],fileinputs[1:])
  # save resulting name
  cx = 0
  while cx<len(fileinputs):
   if not isbad(fileinputs[cx]):
    break
   cx+=1
  if cx<len(fileinputs):
   print '[PASS] Target',t,'reached as',fileinputs[cx]+'.bgf'
   copyfile('bgf/'+fileinputs[cx]+'.bgf','bgf/'+t+'.bgf')
   logwrite('cp bgf/'+fileinputs[cx]+'.bgf bgf/'+t+'.bgf')
  else:
   # Tough luck: all branches failed
   print '[FAIL] Target',t,'unreachable'
  targets[t][1] = t

def isbad(x):
# checks if the file x failed building
 for failed in failedarc:
  if x == '.'.join(failed):
   return True
 return False

def diffall(t,car,cdr):
 if len(cdr)==1:
  run = tools['comparison']+' bgf/'+car+'.bgf bgf/'+cdr[0]+'.bgf'
  logwrite(run)
  if os.system(run+shutup):
   problem = True
   print '[FAIL] Mismatch in target',t+':',car+'.bgf','differs from',cdr[0]+'.bgf'
   failednode.append(t)
   #sysexit(3)
 else:
  for head in cdr:
   diffall(t,car,[head])
  diffall(t,cdr[0],cdr[1:])

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
 tmp = steps[:]
 tmp.reverse()
 print '[PASS] Performed coupled',' '.join(tmp),'on',testcase,
 if treetools.has_key('validation'):
  run = treetools['validation']+' '+re
  if os.system(run+shutup):
   problem = True
   print '- NOT valid',
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
  rturn
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
   open('xbgf/'+a+'.xbgf','r').close()
 except IOError, e:
  print '[FAIL] Undefined action used: need',e.filename
  #sysexit(8)

if __name__ == "__main__":
 print 'Language Covergence Infrastructure v1.12'
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

