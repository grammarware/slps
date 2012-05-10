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
		for e in self.xml.findall(slpsns.cbgf_('*')):
			s = Step()
			s.parse(e)
			self.steps.append(s)
	def addStep(self,s):
		self.steps.append(s)
	def addFirstStep(self,s):
		ns = [s]
		ns.extend(self.steps)
		self.steps = ns
	def getXml(self):
		self.ex = ET.Element(slpsns.cbgf_('relationship'))
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
		self.ex = ET.Element(slpsns.cbgf_(self.name))
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
	def __init__(self,name):
		self.data = name
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

# the nonterminal for rename-renameN
class NonterminalFT:
	def __init__(self,n1,n2):
		self.ntfr = n1
		self.ntto = n2
	def parse(self,nontermelem):
		self.ntfr = nontermelem.findtext('from')
		self.ntto = nontermelem.findtext('to')
	def setFrom(self,name):
		self.ntfr = name
	def setTo(self,name):
		self.ntto = name
	def getXml(self):
		#print 'Getting the XML of nonterminal',self.data,'...'
		self.ex = ET.Element('nonterminal')
		ET.SubElement(self.ex,'from').text = self.ntfr
		ET.SubElement(self.ex,'to'  ).text = self.ntto
		return self.ex
	def __str__(self):
		return self.ntfr+'→'+self.ntto

# the roots for reroot-reroot
class Roots:
	def __init__(self,name,ns):
		self.name = name
		self.ns = ns[:]
	def parse(self,el):
		self.name = el.tag
		self.ns = []
		for nt in el.findall('root'):
			self.ns.append(nt.text)
	def getXml(self):
		#print 'Getting the XML of nonterminal',self.data,'...'
		self.ex = ET.Element(self.name)
		for nt in self.ns:
			ET.SubElement(self.ex,'root').text = nt
		return self.ex
	def __str__(self):
		return ', '.join(self.ns)
