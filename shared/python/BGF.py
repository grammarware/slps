#!/usr/local/bin/python
import os
import sys
sys.path.append('../../../shared/python')
import slpsns
import elementtree.ElementTree as ET

# root::nonterminal* production*
class Grammar:
	def __init__(self):
		self.roots = []
		self.prods = []
	def addRoot(self,r):
		self.roots.append(r)
	def addProd(self,p):
		self.prods.append(p)
	def getXml(self):
		#print 'Getting the XML of grammar...'
		self.ex = ET.Element(slpsns.bgf_('grammar'))
		for e in self.roots:
			ET.SubElement(self.ex,'root').text = e
		for e in self.prods:
			self.ex.append(e.getXml())
		return self.ex
	def __str__(self):
		s = ''
		for e in self.roots:
			s = '[ROOT] '+e+'\n'
		for e in self.prods:
			s += str(e)+'\n'
		return s

# label::label? nonterminal::nonterminal expression
class Production:
	def __init__(self):
		self.label = ''
		self.nt = ''
		self.expr = None
	def setLabel(self,l):
		self.label = l
	def setNT(self,nt):
		self.nt = nt
	def setExpr(self,expr):
		self.expr = expr
	def getXml(self):
		#print 'Getting the XML of production...'
		self.ex = ET.Element(slpsns.bgf_('production'))
		if self.label != '':
			ET.SubElement(self.ex,'label').text = self.label
		ET.SubElement(self.ex,'nonterminal').text = str(self.nt)
		self.ex.append(self.expr.getXml())
		return self.ex
	def __str__(self):
		s = ''
		if self.label != '':
			s = '['+self.label+'] '
		s += str(self.nt)+':\n	'
		s += str(self.expr)
		return s

class Expression:
	def __init__(self,insides):
		self.wrapped = insides
	def getXml(self):
		#print 'Getting the XML of expression...'
		return self.wrapped.getXml()
	def __str__(self):
		return str(self.wrapped)

# terminal::terminal
class Terminal:
	def __init__(self):
		self.data = None
	def setName(self,name):
		self.data = name
	def getXml(self):
		#print 'Getting the XML of terminal...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'terminal').text = self.data
		return self.ex
	def __str__(self):
		return '"'+self.data+'"'

# nonterminal::nonterminal
class Nonterminal:
	def __init__(self):
		self.data = None
	def setName(self,name):
		self.data = name
	def getXml(self):
		#print 'Getting the XML of nonterminal',self.data,'...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'nonterminal').text = self.data
		return self.ex
	def __str__(self):
		return self.data

# selectable::(selector::selector expression)
class Selectable:
	def __init__(self):
		self.sel = None
		self.expr = None
	def setName(self,name):
		self.sel = name
	def setExpr(self,expr):
		self.expr = expr
	def getXml(self):
		#print 'Getting the XML of selectable',self.sel,'...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		self.xml = ET.SubElement(self.ex,'selectable')
		ET.SubElement(self.xml,'selector').text = self.sel
		self.xml.append(self.expr.getXml())
		return self.ex
	def __str__(self):
		return self.sel+'::'+str(self.expr)

# epsilon::EPSILON
class Epsilon:
	def __init__(self):
		pass
	def getXml(self):
		#print 'Getting the XML of epsilon...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'epsilon')
		return self.ex
	def __str__(self):
		return 'EPSILON'

# any::EPSILON
class Any:
	def __init__(self):
		pass
	def getXml(self):
		#print 'Getting the XML of any...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'any')
		return self.ex
	def __str__(self):
		return 'ANY'

# empty::EPSILON
class Empty:
	def __init__(self):
		pass
	def getXml(self):
		#print 'Getting the XML of empty...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'empty')
		return self.ex
	def __str__(self):
		return 'EMPTY'

# value::value
class Value:
	def __init__(self):
		self.data = None
	def setInt(self):
		self.data = 'int'
	def setStr(self):
		self.data = 'string'
	def getXml(self):
		#print 'Getting the XML of value...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'value').text = self.data
		return self.ex
	def __str__(self):
		return self.data.upper()

# sequence::(expression+)
class Sequence:
	def __init__(self):
		self.data = []
	def add(self,expr):
		self.data.append(expr)
	def getXml(self):
		#print 'Getting the XML of sequence...'
		if len(self.data) == 0:
			return None
		elif len(self.data) == 1:
			return self.data[0].getXml()
		else:
			self.ex = ET.Element(slpsns.bgf_('expression'))
			self.xml = ET.SubElement(self.ex,'sequence')
			for el in self.data:
				self.xml.append(el.getXml())
			return self.ex
	def __str__(self):
		s = '('
		for el in self.data:
			s += str(el)+' '
		s += ')'
		return s

# choice::(expression+)
class Choice:
	def __init__(self):
		self.data = []
	def add(self,expr):
		self.data.append(expr)
	def getXml(self):
		#print 'Getting the XML of choice...'
		if len(self.data) == 0:
			return None
		elif len(self.data) == 1:
			return self.data[0].getXml()	
		else:
			self.ex = ET.Element(slpsns.bgf_('expression'))
			self.xml = ET.SubElement(self.ex,'choice')
			for el in self.data:
				self.xml.append(el.getXml())
			return self.ex
	def __str__(self):
		s = '('
		for el in self.data:
			s += str(el)+' | '
		s += ')'
		return s

# marked::expression
class Marked:
	def __init__(self):
		self.data = None
	def setExpr(self,expr):
		self.data = expr
	def getXml(self):
		#print 'Getting the XML of marked...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'marked').append(self.data.getXml())
		return self.ex
	def __str__(self):
		return '<'+str(self.data)+'>'

# optional::expression
class Optional:
	def __init__(self):
		self.data = None
	def setExpr(self,expr):
		self.data = expr
	def getXml(self):
		#print 'Getting the XML of ?...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'optional').append(self.data.getXml())
		return self.ex
	def __str__(self):
		return str(self.data)+'?'

# star::expression
class Star:
	def __init__(self):
		self.data = None
	def setExpr(self,expr):
		self.data = expr
	def getXml(self):
		#print 'Getting the XML of *...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'star').append(self.data.getXml())
		return self.ex
	def __str__(self):
		return str(self.data)+'*'

# plus::expression
class Plus:
	def __init__(self):
		self.data = None
	def setExpr(self,expr):
		self.data = expr
	def getXml(self):
		#print 'Getting the XML of +...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'plus').append(self.data.getXml())
		return self.ex
	def __str__(self):
		return str(self.data)+'+'
