#!/usr/bin/python
import os
import sys
import string
from elementtree import ElementTree

shutup = ' 1> /dev/null 2> /dev/null'
sections = {'SHORTCUTS':1,'ACTIONS':2,'SOURCES':3,'TARGETS':4,'IMPLEMENTATIONS':5}
shortcuts = {}
actions = {}
sources = {}
targets = {}
implementations = {}
testset = []
graph = []
log = None

def logwrite(s):
 log.write(s+'\n')

def sysexit(n):
 log.close()
 sys.exit(n)

def readxmlconfig (cfg):
 config = ElementTree.parse(cfg)
 cx1=cx2=cx3=cx4=cx5=0
 # shortcuts
 for outline in config.findall('//shortcut'):
  cx1+=1
  shortcuts[outline.findtext('name')]=expandxml(outline.findall('definition')[0],{})
 # actions
 for outline in config.findall('//action'):
  for name in outline.findall('name'):
   cmds = []
   for cmd in outline.findall('definition/command'):
    cmds.append(expandxml(cmd,{'action':name.text}))
    if cmd.attrib.has_key('out'):
     cmds[-1]+=' 1> '+cmd.attrib['out']
    if cmd.attrib.has_key('err'):
     cmds[-1]+=' 2> '+cmd.attrib['err']
   actions[name.text]=cmds
   cx2+=1
 # sources
 for outline in config.findall('//source'):
  args = []
  for arg in outline.findall('arguments/argument'):
   args.append(expandxml(arg,{}))
  sources[outline.findtext('name')]=[outline.findtext('type'),' '.join(args)]
  cx3+=1
  pcmds = []
  ecmds = []
  for cmd in outline.findall('parser/command'):
   pcmds.append(expandxml(cmd,{}))
   if cmd.attrib.has_key('out'):
    pcmds[-1]+=' 1> '+cmd.attrib['out']
   if cmd.attrib.has_key('err'):
    pcmds[-1]+=' 2> '+cmd.attrib['err']
  for cmd in outline.findall('evaluator/command'):
   ecmds.append(expandxml(cmd,{}))
   if cmd.attrib.has_key('out'):
    ecmds[-1]+=' 1> '+cmd.attrib['out']
   if cmd.attrib.has_key('err'):
    ecmds[-1]+=' 2> '+cmd.attrib['err']
  if pcmds and ecmds:
   implementations[outline.findtext('name')]=[pcmds,ecmds]
   cx5+=1
 # targets
 for outline in config.findall('//target'):
  name = outline.findtext('name')
  targets[name]= [[],'']
  cx4+=1
  for br in outline.findall('branch'):
   branch = [br.findtext('take')]
   for p in br.findall('perform'):
    branch.append(p.text)
   targets[name][0].append(branch)
 print 'Read',cx1,'shortcuts,',cx2,'actions,',cx3,'sources','('+`cx5`,'implemented)',cx4,'targets.'

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

def readconfig (cfg):
 inside = False
 name = first = ''
 cx1=cx2=cx3=cx4=cx5=0
 config = open(cfg,'r')
 for line in config.readlines():
  line = line.strip()
  if line == '' or line.find('#')==0:
   # comment line
   continue
  if line.isupper():
   # section name
   nowin = sections[line]
   continue
  if nowin == 1:
   # shortcuts
   cut = line.split(' = ')
   shortcuts[cut[0]]=cut[1]
   cx1+=1
  if nowin == 2:
   # actions
   if inside:
    if name.find(',')<0:
     actions[name]=[line.replace('%action%',name)]
     cx2+=1
    else:
     # comma-separated list of actions share the same definition
     for name in map(string.strip,name.split(',')):
      actions[name]=[line.replace('%action%',name)]
      cx2+=1
   else:
    name = line
   inside = not inside
  if nowin == 3:
   # sources
   if inside:
    if first!='':
     sources[name]=[first,line]
     first = ''
     cx3+=1
    else:
     first = line
     continue
   else:
    name = line
   inside = not inside
  if nowin == 4:
   # targets
   if line[-1]==':':
    # new target
    name = line[:-1]
    targets[name] = [[],'']
    cx4+=1
   else:
    branch = line.split()
    branch.reverse()
    targets[name][0].append(branch)
  if nowin == 5:
   # implementations
   if inside:
    if first!='':
     implementations[name]=[[first],[line]]
     first = ''
     cx5+=1
    else:
     first = line
     continue
   else:
    name = line
   inside = not inside
 config.close()
 print 'Read',cx1,'shortcuts,',cx2,'actions,',cx3,'sources','('+`cx5`,'implemented)',cx4,'targets.'
 # expand shortcuts
 for sc in shortcuts.keys():
  shortcuts[sc]=expanduni(shortcuts[sc],{})

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

def addarc(fromnode,tonode,labelnode):
 if [fromnode,tonode,labelnode] not in graph:
  graph.append([fromnode,tonode,labelnode])

def drawchain(chain,tgt):
 if len(chain)==1:
  addarc(chain[0],tgt,'')
 else:
  name = chain[0]
  for i in range(1,len(chain)):
   # going back
   addarc(name,name+"'",chain[i])
   name += "'"
  addarc(name,tgt,'')

def makegraph(df):
 # first we generate a complete picture
 dot = open(df+'_large.dot','w')
 dot.write('digraph generated{ {rank=same;')
 for x in sources.keys():
  dot.write(quote(x)+';')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  for src in targets[x][0]:
   drawchain(src,x)
  dot.write(quote(x)+';')
 dot.write('node [shape=ellipse]\n')
 for arc in graph:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
  if arc[2]:
   dot.write(' [label="'+arc[2]+'"]')
  dot.write(';\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_large.pdf'
 logwrite(run)
 os.system(run)
 g = graph[:]
 for arc in g:
  graph.remove(arc)
 # then we make a simplified one
 dot = open(df+'_small.dot','w')
 dot.write('digraph generated{ {rank=same;')
 for x in sources.keys():
  dot.write(quote(x)+';')
 dot.write('}')
 dot.write('node [shape=octagon]\n')
 for x in targets.keys():
  for src in targets[x][0]:
   addarc(src[0],x,'')
  dot.write(quote(x)+';')
 dot.write('node [shape=ellipse]\n')
 for arc in graph:
  dot.write(quote(arc[0])+'->'+quote(arc[1]))
  if arc[2]:
   dot.write(' [label="'+arc[2]+'"]')
  dot.write(';\n')
 dot.write('}')
 dot.close()
 run = 'dot -Tpdf '+dot.name+' -o '+df+'_small.pdf'
 logwrite(run)
 os.system(run)

def runforall(cmd,prc):
 for lgf in sources.keys():
  for line in actions[cmd]:
   run = expanduni(line,{'source':lgf,'type':sources[lgf][0],'arguments':expanduni(sources[lgf][1],{})})
   logwrite(run)
   ret = os.system(run+shutup)
   if ret!=0:
    print prc,'failed on',lgf
    print 'Command was:',run
    sysexit(3)
 print prc,'successful.'

def preparelgf(cut):
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
  # action names will be appended, so 'destroy confuse corrupt x' will yield
  # files x.lgf, x.corrupt.lgf, x.corrupt.confuse.lgf and x.corrupt.confuse.destroy.lgf
  # the very last one will be diffed
  for a in cut[1:]:
   if a not in actions.keys():
    print 'Action',a,'needed for',curname,'not found.'
    sysexit(5)
   for linetorun in actions[a]:
    run = expanduni(linetorun,{'source':curname})
    logwrite(run)
    ret = os.system(run+shutup)
    if ret!=0:
     print a,'failed on',curname
     print 'Command was:',run
     sysexit(4)
   curname += '.'+a
 a=cut[:]
 a.reverse()
 name = ' '.join(a)
 if actions.has_key('validate'):
  a = expanduni(actions['validate'][0],{'source':curname})
  logwrite(a)
  ret = os.system(a+shutup)
  print 'Successfully performed',name,'- the result is',
  if ret!=0:
   print 'NOT',
  print 'valid'
 else:
  print 'Successfully performed',name
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
   fileinputs[i] = preparelgf(inputs[i])
  if len(inputs)>1:
   # need to diff
   diffall(t,fileinputs[0],fileinputs[1:])
  # save resulting name
  targets[t][1] = fileinputs[0]

def diffall(t,car,cdr):
 if len(cdr)==1:
  run = expanduni(actions['diff'][0],{'first':car,'second':cdr[0]})
  logwrite(run)
  ret = os.system(run+shutup)
  if ret!=0:
   print 'Error occured building target',t,'-',car,'differs from',cdr[0]
   sysexit(3)
 else:
  for head in cdr:
   diffall(t,car,[head])
  diffall(t,cdr[0],cdr[1:])

def unpacksamples():
 for line in actions['test']:
  run = expanduni(line,{})
  logwrite(run)
  ret = os.system(run)
  if ret!=0:
   print 'Test set extraction failed'
   sysexit(6)
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
    results[program]=0
    for cmd in implementations[program][1]:
     run = expanduni(cmd,{'sample':testcase[0],'context':testcase[1],'yields':testcase[2]})
     logwrite(run)
     results[program]+=os.system(run)
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
    results[program]=0
    for cmd in implementations[program][0]:
     run = expanduni(cmd,{'sample':testcase[0],'parsed':testcase[0]+'.parsed'})
     logwrite(run)
     results[program]+=os.system(run)
   print 'Test case',testcase[0],
   if results.values()==[0]*len(implementations):
    # all zeros
    print 'passed'
   else:
    print 'failed'
    for r in results.keys():
     if results[r]!=0:
      print r,'could not parse it'

def checkconsistency():
 # some simple assertions
 try:
  # all targets depend on existing targets or sources
  for t in targets.keys():
   for i in targets[t][0]:
    if not (targets.has_key(i[0]) or sources.has_key(i[0])):
     print 'Target',t,'needs',i[0],'which is not defined'
     raise Exception
 except:
  sysexit(7)

if __name__ == "__main__":
 print 'Language Covergence Infrastructure v1.3'
 if len(sys.argv) == 4:
  log = open(sys.argv[2].split('.')[0]+'.log','w')
  if sys.argv[1]=='xml':
   readxmlconfig(sys.argv[2])
  else:
   readconfig(sys.argv[2])
  checkconsistency()
  makegraph(sys.argv[3])
  runforall('extract', 'Extraction')
  if actions.has_key('validate'):
   runforall('validate','Validation')
  buildtargets()
  print 'Grammar convergence ended successfully.'
  if actions.has_key('test'):
   unpacksamples()
   runtestset()
   print 'Testing ended successfully.'
  else:
   print 'No testing performed.'
  log.close()
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration type>','<configuration file>','<output pdf>'
  print 'Configuration type can be "xml" (for LCF) or something else (for text config).'
  sysexit(1)

