#!/usr/bin/python
import os
import sys
from elementtree import ElementTree

shutup = ' 1> /dev/null 2> /dev/null'
shortcuts = {}
actions = []
sources = {}
targets = {}
implementations = {}
testset = []
graph_big = []
graph_small = []
log = None
tools = {}
failednode = []
failedarc  = []

def logwrite(s):
 log.write(s+'\n')
 log.flush()

def sysexit(n):
 log.close()
 sys.exit(n)

def readxmlconfig (cfg):
 config = ElementTree.parse(cfg)
 # shortcuts
 for outline in config.findall('//shortcut'):
  shortcuts[outline.findtext('name')]=expandxml(outline.findall('expansion')[0],{})
 # actions
 for outline in config.findall('//target/branch/perform'):
  if outline.text not in actions:
   actions.append(outline.text)
 # sources
 for outline in config.findall('//source'):
  args = [expandxml(outline.findall('extraction/name')[0],{})]
  for arg in outline.findall('extraction/argument'):
   args.append(expandxml(arg,{}))
  sources[outline.findtext('name')]=args
  pcmd = ecmd = ''
  if outline.findall('parsing/command'):
   pcmd = expandxml(outline.findall('parsing/command')[0],{})
  if outline.findall('evaluation/command'):
   ecmd = expandxml(outline.findall('evaluation/command')[0],{})
  if pcmd and ecmd:
   implementations[outline.findtext('name')]=[pcmd,ecmd]
 # targets
 for outline in config.findall('//target'):
  name = outline.findtext('name')
  targets[name]= [[],'']
  for br in outline.findall('branch'):
   branch = [br.findtext('input')]
   for p in br.findall('perform'):
    branch.append(p.text)
   targets[name][0].append(branch)
 # tools
 for outline in config.findall('//tool'):
  cmd = outline.findall('command')[0]
  line = expandxml(cmd,{})
  if cmd.attrib.has_key('out'):
   line += ' 1> '+cmd.attrib['out']
  if cmd.attrib.has_key('err'):
   line += ' 2> '+cmd.attrib['err']
  tools[outline.findtext('name')]=line

 print 'Read',len(shortcuts),'shortcuts,',len(tools),'tools,',len(actions),'actions,',
 print len(sources),'sources','('+`len(implementations)`,'implemented),',len(targets),'targets.'

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
    print 'Misused expand, referencing undefined "'+cut[i]+'":'
    print '?????',where
    sysexit(11)
 return ''.join(cut)

def quote(a):
 return '"'+a+'"'

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
     addarc(name,name+"'",qname,src[i])
     qname += '.'+src[i]
     name += "'"
    addarc(name,x,qname,src[-1])
 # make a simplified one
 for x in targets.keys():
  for src in targets[x][0]:
   graph_small.append([src[0],x])

def dumpgraph(df):
 dot = open(df+'_large.dot','w')
 dot.write('digraph generated{ {rank=same; node [shape=ellipse];')
 for x in sources.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('}\n')
 dot.write('node [shape=octagon];\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('node [shape=box];\n')
 nodezz=[]
 for arc in graph_big:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
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
  if node not in sources.keys():
   if node not in targets.keys():
    if node in failednode:
     dot.write(quote(node)+' [color=red];')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_large.pdf'
 logwrite(run)
 os.system(run)
 dot = open(df+'_small.dot','w')
 dot.write('digraph generated{ {rank=same;')
 for x in sources.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
  dot.write(';')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  dot.write(quote(x))
  if x in failednode:
   dot.write(' [color=red]')
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
 os.system(run)

def extractall():
 for bgf in sources.keys():
  run = ' '.join(sources[bgf])
  logwrite(run)
  if os.system(run+shutup):
   print 'Extraction failed on',bgf
   failednode.append(bgf)
   #sysexit(3)
 print 'Extraction finished.'

def validateall():
 for bgf in sources.keys():
  if bgf in failednode:
   continue
  run = tools['validation']+' '+bgf+'.bgf'
  logwrite(run)
  if os.system(run+shutup):
   print 'Validation failed on',bgf
   failednode.append(bgf)
   #sysexit(3)
 print 'Validation finished.'

def preparebgf(cut):
 # executes preparational actions (abstract, unerase, etc) before comparison
 if len(cut)==1:
  return cut[0]
 else:
  if cut[0] in sources.keys():
   # starting point is a source
   curname = cut[0]
  else:
   # starting point is another target
   curname = targets[cut[0]][1]
  # action names will be appended:
  # x.bgf -> x.corrupt.bgf -> x.corrupt.confuse.bgf -> x.corrupt.confuse.destroy.bgf -> ...
  # the very last one will be diffed
  for a in cut[1:]:
   run = tools['transformation']+' '+curname+'.bgf xbgf/'+a+'.xbgf '+curname+'.'+a+'.bgf'
   logwrite(run)
   if os.system(run+shutup):
    print a,'failed on',curname
    failedarc.append([curname,a])
    failednode.append(cut[0]+"'"*(curname.count('.')+1))
    #sysexit(4)
   curname += '.'+a
 a=cut[:]
 a.reverse()
 name = ' '.join(a)
 if tools.has_key('validation'):
  a = tools['validation']+' '+curname+'.bgf'
  logwrite(a)
  print 'Performed',name,'- the result is',
  if os.system(a+shutup):
   print 'NOT',
  print 'valid'
 else:
  print 'Performed',name
 return curname

def buildtargets():
 unordered = targets.keys()
 ordered = []
 while len(unordered):
  for t in unordered:
   flag = True
   for i in targets[t][0]:
    if (i[0] not in ordered) and (i[0] not in sources.keys()):
     flag = False
   if flag:
    ordered.append(t)
    unordered.remove(t)
 for t in ordered:
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
   print 'Target',t,'reached as',fileinputs[cx]
   targets[t][1] = fileinputs[cx]
  else:
   # Tough luck: all branches failed
   targets[t][1] = t

def isbad(x):
# checks if the file x failed building
 for failed in failedarc:
  if x == '.'.join(failed):
   return True
 return False

def diffall(t,car,cdr):
 if len(cdr)==1:
  run = tools['comparison']+' '+car+'.bgf '+cdr[0]+'.bgf'
  logwrite(run)
  ret = os.system(run+shutup)
  if ret!=0:
   print 'Error occured building target',t,'-',car,'differs from',cdr[0]
   failednode.append(t)
   #sysexit(3)
 else:
  for head in cdr:
   diffall(t,car,[head])
  diffall(t,cdr[0],cdr[1:])

def unpacksamples():
 run = expanduni(tools['testset'],{})+' samples.xml'
 logwrite(run)
 if os.system(run+shutup):
  print 'Test set extraction failed, no cases tested'
  #sysexit(6)
  return
 library={}
 cx = 0
 tree = ElementTree.parse('samples.xml')
 for outline in tree.findall("//sample"):
  cx+=1
  torun = open ('sample'+`cx`,"w")
  for line in outline.text.split('\n'):
   if line.strip()!='':
    torun.write(line.strip()+'\n')
  torun.close()
  if outline.attrib.has_key('id'):
   library[outline.attrib['id']]=outline.text
  if outline.attrib.has_key('sort'):
   sort=outline.attrib['sort']
  else:
   sort=None
  testset.append(['sample'+`cx`,None,None,sort])
 # All executions
 for outline in tree.findall("//runnable"):
  cx+=1
  # sample,context,yields,sort
  yields=None
  context=None
  torun = open ('sample'+`cx`,'w')
  line = outline.findtext('main')
  for arg in outline.findall("argument"):
   line += ' ' + arg.text
  torun.write(line+'\n')
  torun.close()
  if outline.findtext('yields'):
   yields=outline.findtext('yields')
  if outline.findtext('context'):
   if library.has_key(outline.findtext('context')):
    context='sample'+`cx`+'.context'
    con = open (context,'w')
    for line in library[outline.findtext('context')].split('\n'):
     if line.strip()!='':
      con.write(line.strip()+'\n')
    con.close()
   else:
    print "No context found for sample",cx,'('+outline.findtext('context')+'), test case not used'
    continue
  testset.append(['sample'+`cx`,context,yields,''])
 print cx,'samples in the test set.'

def runtestset():
 for testcase in testset:
  if testcase[3]:
   # sort explicitly given
   print 'Test case',testcase[0],'skipped for the lack of functionality to test samples of sort',testcase[3]
  elif testcase[1]:
   # evaluate if context is given
   results = {}
   for program in implementations.keys():
    run = expanduni(implementations[program][1]+' '+testcase[1]+' '+testcase[0]+' '+testcase[2],{})
    logwrite(run)
    results[program]=os.system(run+shutup)
   print 'Test case',testcase[0],
   if results.values()==[0]*len(implementations):
    # all zeros
    print 'passed'
   else:
    print 'failed'
    for r in results.keys():
     if results[r]!=0:
      print r,'evaluated it differently'
  else:
   # parse otherwise
   results = {}
   for program in implementations.keys():
    run = expanduni(implementations[program][0]+' '+testcase[0]+' '+testcase[0]+'.parsed',{})
    logwrite(run)
    results[program]=os.system(run+shutup)
    if not results[program]:
     # parsed successfully, let's check the result
     run = 'diff '+testcase[0]+' '+testcase[0]+'.parsed'
     logwrite(run)
     results[program]=os.system(run+shutup)
   print 'Test case',testcase[0],
   if results.values()==[0]*len(implementations):
    # all zeros
    print 'passed'
   else:
    print 'failed'
    for r in results.keys():
     if results[r]!=0:
      print r,'did not parse it correctly'

def checkconsistency():
 # some simple assertions
 # all targets depend on existing targets or sources
 for t in targets.keys():
  for i in targets[t][0]:
   if not (targets.has_key(i[0]) or sources.has_key(i[0])):
    print 'Target',t,'needs',i[0],'which is not defined'
    sysexit(7)
 # all actions can be found
 try:
  for a in actions:
   open('xbgf/'+a+'.xbgf','r').close()
 except IOError, e:
  print 'Undefined action used: need',e.filename
  #sysexit(8)

if __name__ == "__main__":
 print 'Language Covergence Infrastructure v1.71'
 if len(sys.argv) == 3:
  log = open(sys.argv[1].split('.')[0]+'.log','w')
  readxmlconfig(sys.argv[1])
  checkconsistency()
  makegraph()
  extractall()
  if tools.has_key('validation'):
   validateall()
  buildtargets()
  print 'Grammar convergence finished.'
  if tools.has_key('testset'):
   unpacksamples()
   runtestset()
   print 'Testing finished.'
  else:
   print 'No testing performed.'
  dumpgraph(sys.argv[2])
  #print failednode
  #print failedarc
  log.close()
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration file>','<output pdf>'
  sysexit(1)

