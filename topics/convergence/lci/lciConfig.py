#!/usr/bin/python
# -*- coding: utf-8 -*-
from elementtree import ElementTree

# transformation type per action: preparation, nominal-matching, normalizing, structural-matching,
#								  extension, correction, relaxation
ttype = {}
orderedsrc = []
derived = {}
shortcuts = {}
actions = []
autoactions = {}
testsets = {}
tester = {}
extractor = {}
treeExtractor = {}
treeEvaluator = {}
targets = {}
parser = {}
evaluator = {}
testset = []
tools = {}
treeTools = {}
automethods = {}

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
 def __len__(self):
  return len(self.array)
 def __eq__(self,other):
  if type(other)==type(''):
   return self.array==other.split('.')
  else:
   return self.array==other.array
 def dotNodeName(self,target):
  name = self.array[0]
  for a in self.array[1:]:
   name += '_'+stripSpecifics(a)
  return name+'_'+target
 def bgfFileName(self):
  name = [self.array[0]]
  for a in self.array[1:]:
   if name[-1].isdigit():
    if name[-2]==stripCamelCase(a):
     name[-1] = str(int(name[-1])+1)
    else:
     name.append(stripCamelCase(a))
   elif name[-1]==stripCamelCase(a):
    name.append('2')
   else:
    name.append(stripCamelCase(a))
  return '.'.join(name)+'.bgf'
 def futureBgfFileName(self,next):
  tmp = self.array[:]
  self.array.append(stripCamelCase(next))
  name = self.bgfFileName()
  self.array = tmp
  return name
 def append(self,step):
  self.array.append(step)
 def spaceNotation(self):
  tmp = self.array[:]
  tmp.reverse()
  return ' '.join(tmp)
 def type(self):
  t = 0
  for a in self.array[1:]:
   if ttype[a] in ('preparation','nominal-matching','normalizing'):
    t = -1
   if ttype[a] in ('structural-matching','extension','correction','relaxation'):
    t = 1
  return t
 def lastAction(self):
  return self.array[-1]

def ChainFromArray(a):
 x = Chain()
 x.array = a[:]
 return x

def readConfiguration (cfg):
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
  orderedsrc.append(xmlnode.findtext('name'))
  if xmlnode.findall('derived'):
   derived[xmlnode.findtext('name')]=(xmlnode.findtext('derived/from'),xmlnode.findtext('derived/using'))
  extractor[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/extraction')[0],{})
  if xmlnode.findall('grammar/parsing'):
   parser[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/parsing')[0],{})
  if xmlnode.findall('grammar/evaluation'):
   evaluator[xmlnode.findtext('name')]=expandxml(xmlnode.findall('grammar/evaluation')[0],{})
  if xmlnode.findall('tree/extraction'):
   treeExtractor[xmlnode.findtext('name')]=expandxml(xmlnode.findall('tree/extraction')[0],{})
  if xmlnode.findall('tree/evaluation'):
   treeEvaluator[xmlnode.findtext('name')]=expandxml(xmlnode.findall('tree/evaluation')[0],{})
  tmp = []
  for theset in xmlnode.findall('testing/set'):
   tmp.append(theset.text)
  tester[xmlnode.findtext('name')]=tmp[:]
 # targets
 for xmlnode in config.findall('//target'):
  name = xmlnode.findtext('name')
  targets[name]= []
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
   targets[name].append(branch)
 # tools & methods
 for xmlnode in config.findall('//tools/*'):
  #print 'Processing tool',xmlnode.tag
  if xmlnode.tag == 'generator':
   automethods[xmlnode.findtext('name')] = expandxml(xmlnode.findall('command')[0],{})
  else:
   tools[xmlnode.tag] = expandxml(xmlnode.findall('grammar')[0],{})
   if xmlnode.findall('tree'):
    treeTools[xmlnode.tag] = expandxml(xmlnode.findall('tree')[0],{})
 print 'Read',
 if shortcuts:
  print len(shortcuts),'shortcuts,',
 if tools or treeTools:
  print `len(tools)`+'+'+`len(treeTools)`,'tools,',
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

def expandxml(mixed,rep):
 s = mixed.text
 if not s:
  s = ''
 for tag in mixed.getchildren():
  s += expandone(tag.tag,tag.text,rep)
  s += tag.tail
 return s.strip()

def stripSpecifics(lbl):
 l = lbl[:]
 if l.find('-')>0:
  l = l.split('-')[0]
 return l

def stripCamelCase(lbl):
 l=''
 for x in lbl:
  if x.islower() or x=='.':
   l+=x
  else:
   break
 return l
