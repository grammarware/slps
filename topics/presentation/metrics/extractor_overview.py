#!/usr/bin/python
import os
import sys
import glob
from elementtree import ElementTree

# A global flag, if set, LCI will exit with a non-zero status
problem = False

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

def texwrite(tex,text):
 tex.write(text+'\n')
 tex.flush()

def reopenafter(tex,fname,cmd):
 tex.close()
 os.system(cmd+' 1>> '+fname+' 2> /dev/null')
 return open(fname,'a')

def sysexit(n):
 log.close()
 sys.exit(n)

def readxmlconfig(path,cfg):
 config = ElementTree.parse(cfg)
 # shortcuts
 for xmlnode in config.findall('//shortcut'):
  shortcuts[xmlnode.findtext('name')] = expandxml(xmlnode.findall('expansion')[0],{})
 for s in shortcuts.keys():
  shortcuts[s] = path + shortcuts[s]
 # actions
 for xmlnode in config.findall('//target/branch/perform'):
  if xmlnode.text not in actions:
   actions.append(xmlnode.text)
 # testset
 for xmlnode in config.findall('//testset'):
  testsets[xmlnode.findtext('name')]=expandxml(xmlnode.findall('command')[0],{})
 # sources
 for xmlnode in config.findall('//source'):
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

def extractall(path):
 for bgf in extractor.keys():
  run = extractor[bgf]+' '+path+'bgf/'+bgf+'.bgf 1> '+bgf+'.data 2> /dev/null'
  if os.system(run):
   print '[FAIL] Extraction of',bgf+'.bgf failed:'
   print run
   sys.exit(3)

if __name__ == "__main__":
 print 'Extractors overview tool: takes LCF, produces LaTeX'
 if len(sys.argv) == 4:
  localpath = sys.argv[3]
  if localpath[-1]!='/':
   localpath+='/'
  tblname = sys.argv[1].split('/')[-1].split('.')[0]+'table.tex'
  tbl = open(tblname,'w')
  readxmlconfig(localpath,sys.argv[1])
  o_txt=[]
  o_kwd=[]
  tok = open(sys.argv[2],'r')
  for x in tok.readlines():
   a,b=x.strip().split(':')
   o_txt.append(a)
   o_kwd.append(b)
  tok.close()
  extractall(localpath)
  texwrite(tbl,'\\begin{tabular}{l'+('|c'*len(extractor))+'||c}\n')
  sorted = extractor.keys()[:]
  sorted.sort()
  for src in sorted:
   texwrite(tbl,'&\\textbf{'+src+'}')
  texwrite(tbl,'&\\textbf{Total}\\\\\\hline')
  for i in range(0,len(o_txt)):
   texwrite(tbl,o_txt[i])
   total = 'expr 0'
   for src in sorted:
    texwrite(tbl,'&')
    tbl = reopenafter(tbl,tblname,'grep -c "'+o_kwd[i]+'" '+src+'.data')
    total += ' + `grep -c "'+o_kwd[i]+'" '+src+'.data`'
   texwrite(tbl,'&')
   tbl = reopenafter(tbl,tblname,total)
   texwrite(tbl,'\\\\')
  texwrite(tbl,'\\hline\\end{tabular}')
  sys.exit(0)
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<configuration file>','<keyword file>','<path to LCI>'
  sys.exit(1)

