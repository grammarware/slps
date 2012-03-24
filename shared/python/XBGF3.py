#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
import os
import sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import slpsns
import BGF3
import xml.etree.ElementTree as ET

class Sequence:
	def __init__(self):
		self.steps = []
	def parse(self,fname):
		self.steps = []
		self.xml = ET.parse(fname)
		for e in self.xml.findall(slpsns.xbgf_('*')):
			s = Step()
			s.parse(e)
			self.steps.append(s)
	def addStep(self,s):
		self.steps.append(s)
	def getXml(self):
		self.ex = ET.Element(slpsns.xbgf_('sequence'))
		for e in self.steps:
			self.ex.append(e.getXml())
		return self.ex

class Step:
	def __init__(self,op):
		self.name = op
		self.params = []
	def parse(self,ee):
		self.name = ee.tag
		for e in e.findall(slpsns.bgf_('*')):
			if e.tag == 'expression':
				ne = BGF3.Expression()
			elif e.tag == 'production':
				ne = BGF3.Production()
			else:
				print('Unknown parameter of type',e.tag)
				ne = None
			ne.parse(e)
			self.params.append(ne)
	def setName(self,n):
		self.name = n
	def addParam(self,p):
		self.params.append(p)
	def getXml(self):
		#print 'Getting the XML of production...'
		self.ex = ET.Element(slpsns.xbgf_(self.name))
		for p in self.params:
			self.ex.append(p.getXml())
		return self.ex

class Label:
	def __init__(self,n):
		self.name = n
	def getXml(self):
		e = ET.Element('label')
		e.text = self.name
		return e
	def __str__(self):
		return self.name

class Root:
	def __init__(self,n):
		self.name = n
	def getXml(self):
		e = ET.Element('root')
		e.text = self.name
		return e
	def __str__(self):
		return self.name

# the main difference from BGF3.Nonterminal is the absence of wrapping expression
class Nonterminal:
	def __init__(self):
		self.data = None
	def parse(self,nontermelem):
		self.data = nontermelem.text
	def setName(self,name):
		self.data = name
	def getXml(self):
		#print 'Getting the XML of nonterminal',self.data,'...'
		self.ex = ET.Element('nonterminal')
		self.ex.text = self.data
		return self.ex
	def __str__(self):
		return self.data

# nonterminal in renameN, vertical in addV, etc
class Wrapping:
	def __init__(self,name):
		self.name = name
		self.data = []
	def parse(self,welem):
		self.name = welem.tag
		self.data = []
		for e in welem.findall('*'):
			self.data.append(XBGF3.Leaf(e.tag,e.text))
			# self.data[-1].parse(e)
	def setName(self,name):
		self.name = name
	def addChild(self,c):
		self.data.append(c)
	def getXml(self):
		self.ex = ET.Element(self.name)
		for e in self.data:
			self.ex.append(e.getXml())
		return self.ex
	def __str__(self):
		return self.name+'('+self.data+')'

# from, to, etc
class Leaf:
	def __init__(self,name,data):
		self.name = name
		self.data = data
	def parse(self,welem):
		self.name = welem.tag
		self.data = welem.findtext()
	def setName(self,name):
		self.name = name
	def setValue(self,name):
		self.data = name
	def getXml(self):
		self.ex = ET.Element(self.name)
		self.ex.text = self.data
		return self.ex
	def __str__(self):
		return self.name+':'+self.data
