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
    cx2+=1
    if name.find(',')<0:
     actions[name]=line.replace('%action%',name)
    else:
     # comma-separated list of actions share the same definition
     for name in map(string.strip,name.split(',')):
      actions[name]=line.replace('%action%',name)
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
    targets[name][0].append(line)
  if nowin == 5:
   # implementations
   if inside:
    if first!='':
     implementations[name]=[first,line]
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
  shortcuts[sc]=expand(shortcuts[sc])

def expand(where):
 cut = where.split('%')
 for i in range(0,len(cut)):
  if i%2:
   try:
    cut[i]=shortcuts[cut[i]]
   except:
    print 'Shortcut definition error, referencing undefined "'+cut[i]+'":'
    print '?????',where
    sys.exit(2)
 return ''.join(cut)

def expandnametypeargs(where,n,t,a):
 cut = where.split('%')
 for i in range(0,len(cut)):
  if i%2:
   if shortcuts.has_key(cut[i]):
    # regular shortcut
    cut[i]=shortcuts[cut[i]]
   else:
    # name, type or args
    if cut[i]=='name':
     cut[i]=n
    elif cut[i]=='type':
     cut[i]=t
    elif cut[i]=='args':
     cut[i]=a
    else:
     print 'Shortcut definition error, referencing undefined "'+cut[i]+'"!'
     print '?????',where
     sys.exit(2)
 return ''.join(cut)

def expandsamplecontextyields(where,s,c,y):
 cut = where.split('%')
 for i in range(0,len(cut)):
  if i%2:
   if shortcuts.has_key(cut[i]):
    # regular shortcut
    cut[i]=shortcuts[cut[i]]
   else:
    # sample, parsed, context or yields
    if cut[i]=='sample':
     cut[i]=s
    elif cut[i]=='parsed':
     cut[i]=s+'.parsed'
    elif cut[i]=='context':
     cut[i]=c
    elif cut[i]=='yields':
     cut[i]=y
    else:
     print 'Shortcut definition error, referencing undefined "'+cut[i]+'"!'
     print '?????',where
     sys.exit(2)
 return ''.join(cut)

def quote(a):
 return '"'+a+'"'

def addarc(fromnode,tonode,labelnode):
 if [fromnode,tonode,labelnode] not in graph:
  graph.append([fromnode,tonode,labelnode])

def drawchain(src,tgt):
 chain = src.split()
 if len(chain)==1:
  addarc(src,tgt,'')
 else:
  name = chain[-1]
  lines = ''
  for i in range(len(chain)-2,-1,-1):
   # going back
   addarc(name,name+"'",chain[i])
   name += "'"
  addarc(name,tgt,'')

def makegraph(dotfile):
 dot = open(dotfile.split('.')[0]+'.dot','w')
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
 ret = os.system('dot -Tpdf '+dot.name+' -o '+dotfile)

def runforall(cmd,prc):
 for lgf in sources.keys():
  ret = os.system(expandnametypeargs(actions[cmd],lgf,sources[lgf][0],expand(sources[lgf][1]))+shutup)
  if ret!=0:
   print prc,'failed on',lgf
   sys.exit(3)
 print prc,'successful.'

def preparelgf(name):
 # executes preparational actions (abstract, unerase, etc) before comparison
 cut = name.split()
 if len(cut)==1:
  return name
 else:
  cut.reverse()
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
    sys.exit(5)
   ret = os.system(expand(actions[a].replace('%name%',curname))+shutup)
   if ret!=0:
    print a,'failed on',curname
    sys.exit(4)
   curname += '.'+a
 if actions.has_key('validate'):
  ret = os.system(expand(actions['validate'].replace('%name%',curname))+shutup)
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
    name = i.split()[-1]
    if (name not in ordered) and (name not in sources.keys()):
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
  ret = os.system(expand(actions['diff'].replace('%1%',car).replace('%2%',cdr[0]))+shutup)
  if ret!=0:
   print 'Error occured building target',t,'-',car,'differs from',cdr[0]
   sys.exit(3)
 else:
  for head in cdr:
   diffall(t,car,[head])
  diffall(t,cdr[0],cdr[1:])

def unpacksamples():
 ret = os.system(expand(actions['test']))
 if ret!=0:
  print 'Test set extraction failed'
  sys.exit(6)
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
    results[program]=os.system(expandsamplecontextyields(implementations[program][1],testcase[0],testcase[1],testcase[2]))
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
    results[program]=os.system(expandsamplecontextyields(implementations[program][0],testcase[0],'',''))
   print 'Test case',testcase[0],
   if results.values()==[0]*len(implementations):
    # all zeros
    print 'passed'
   else:
    print 'failed'
    for r in results.keys():
     if results[r]!=0:
      print r,'could not parse it'

if __name__ == "__main__":
 if len(sys.argv) == 3:
  readconfig(sys.argv[1])
  makegraph(sys.argv[2])
  runforall('extract', 'Extraction')
  if actions.has_key('validate'):
   runforall('validate','Validation')
  buildtargets()
  print 'Grammar synchronisation ended successfully.'
  if actions.has_key('test'):
   unpacksamples()
   runtestset()
   print 'Testing ended successfully.'
  else:
   print 'No testing performed.'
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration file>','<output pdf>'
  sys.exit(1)

