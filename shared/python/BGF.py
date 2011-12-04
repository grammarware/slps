#!/usr/local/bin/python
import os
import sys
import slpsns
import elementtree.ElementTree as ET

# root::nonterminal* production*
class Grammar:
	def __init__(self):
		self.roots = []
		self.prods = []
	def parse(self,fname):
		self.roots = []
		self.prods = []
		self.xml = ET.parse(fname)
		for e in self.xml.findall('root'):
			self.roots.append(e.text)
		for e in self.xml.findall(slpsns.bgf_('production')):
			prod = Production()
			prod.parse(e)
			self.prods.append(prod)
	def addRoot(self,r):
		self.roots.append(r)
	def addProd(self,p):
		self.prods.append(p)
	def getProdsOfN(self,n):
		return filter(lambda x:x.nt==n,self.prods)
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
		#for e in self.roots:
		#	s = '[ROOT] '+e+'\n'
		for e in self.prods:
			s += str(e)+'\n'
		return s

# label::label? nonterminal::nonterminal expression
class Production:
	def __init__(self):
		self.label = ''
		self.nt = ''
		self.expr = None
	def parse(self,prodelem):
		if prodelem.findall('label'):
			self.label = prodelem.findtext('label')
		else:
			self.label = ''
		self.nt = prodelem.findtext('nonterminal')
		self.expr = Expression(None)
		self.expr.parse(prodelem.findall(slpsns.bgf_('expression'))[0])
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
		s += str(self.nt)+':\n        '
		if self.expr.__class__.__name__ == 'Expression':
			e = self.expr.wrapped
		else:
			e = self.expr
		if e.__class__.__name__ == 'Sequence':
			s += e.unbracketed()
		elif e.__class__.__name__ == 'Choice':
			a = e.asArray()
			for i in range(0,len(a)):
				if a[i][0] == '(' and a[i][-1] == ')':
					a[i] = a[i][1:-1]
			s += '\n        '.join(a)
		else:
			s += str(self.expr)
		return s

class Expression:
	def __init__(self,insides):
		self.wrapped = insides
	def parse(self,exprelem):
		expr = exprelem.findall('*')[0]
		if expr.tag == 'terminal':
			self.wrapped = Terminal()
		elif expr.tag == 'nonterminal':
			self.wrapped = Nonterminal()
		elif expr.tag == 'selectable':
			self.wrapped = Selectable()
		elif expr.tag == 'epsilon':
			self.wrapped = Epsilon()
		elif expr.tag == 'any':
			self.wrapped = Any()
		elif expr.tag == 'empty':
			self.wrapped = Empty()
		elif expr.tag == 'value':
			self.wrapped = Value()
		elif expr.tag == 'sequence':
			self.wrapped = Sequence()
		elif expr.tag == 'choice':
			self.wrapped = Choice()
		elif expr.tag == 'marked':
			self.wrapped = Marked()
		elif expr.tag == 'optional':
			self.wrapped = Optional()
		elif expr.tag == 'plus':
			self.wrapped = Plus()
		elif expr.tag == 'star':
			self.wrapped = Star()
		else:
			print "Don't know how to parse",expr.tag
			return
		self.wrapped.parse(expr)
	def getXml(self):
		#print 'Getting the XML of expression...'
		return self.wrapped.getXml()
	def __str__(self):
		return str(self.wrapped)

# terminal::terminal
class Terminal:
	def __init__(self):
		self.data = None
	def parse(self,termelem):
		self.data = termelem.text
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
	def parse(self,nontermelem):
		self.data = nontermelem.text
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
	def parse(self,selelem):
		self.sel = selelem.findtext('selector')
		self.expr = Expression(None)
		self.expr.parse(selelem.findall(slpsns.bgf_('expression'))[0])
	def setName(self,name):
		self.sel = name
	def setExpr(self,expr):
		self.expr = expr
	def getXml(self):
		#print 'Getting the XML of selectable',self.sel,'...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		self.xml = ET.SubElement(self.ex,'selectable')
		ET.SubElement(self.xml,'selector').text = self.sel
		if self.expr:
			self.xml.append(self.expr.getXml())
			# troubleshooting?
		return self.ex
	def __str__(self):
		name = self.expr.__class__.__name__
		if name == 'Expression':
			name = self.expr.wrapped.__class__.__name__
		#print name
		if name in ('Plus','Star','Optional'):
			return self.sel+'::('+str(self.expr)+')'
		else:
			return self.sel+'::'+str(self.expr)

# epsilon::EPSILON
class Epsilon:
	def __init__(self):
		pass
	def parse(self,e):
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
	def parse(self,e):
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
	def parse(self,e):
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
	def parse(self,valelem):
		self.data = valelem.text
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
		return self.data.upper()[:3]

# sequence::(expression+)
class Sequence:
	def __init__(self):
		self.data = []
	def parse(self,seqelem):
		self.data = []
		for e in seqelem.findall('*'):
			self.data.append(Expression(None))
			self.data[-1].parse(e)
	def add(self,expr):
		self.data.append(expr)
	def getXml(self):
		#print 'Getting the XML of sequence...'
		if len(self.data) == 0:
			return Epsilon().getXml()
		elif len(self.data) == 1:
			return self.data[0].getXml()
		else:
			self.ex = ET.Element(slpsns.bgf_('expression'))
			self.xml = ET.SubElement(self.ex,'sequence')
			for el in self.data:
				self.xml.append(el.getXml())
			return self.ex
	def __str__(self):
		return '(' + self.unbracketed() + ')'
	def unbracketed(self):
		s = ''
		for el in self.data:
			s += str(el)+' '
		return s.strip()

# choice::(expression+)
class Choice:
	def __init__(self):
		self.data = []
	def parse(self,chelem):
		self.data = []
		for e in chelem.findall('*'):
			self.data.append(Expression(None))
			self.data[-1].parse(e)
	def add(self,expr):
		self.data.append(expr)
	def getXml(self):
		#print 'Getting the XML of choice...'
		if len(self.data) == 0:
			return Empty().getXml()
		elif len(self.data) == 1:
			return self.data[0].getXml()	
		else:
			self.ex = ET.Element(slpsns.bgf_('expression'))
			self.xml = ET.SubElement(self.ex,'choice')
			for el in self.data:
				self.xml.append(el.getXml())
			return self.ex
	def __str__(self):
		return '('+' | '.join(self.asArray())+')'
	def asArray(self):
		a = []
		for el in self.data:
			a.append(str(el))
		return a

# marked::expression
class Marked:
	def __init__(self):
		self.data = None
	def parse(self,melem):
		self.data = Expression(None)
		self.data.parse(melem.findall('*')[0])
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
	def parse(self,optelem):
		self.data = Expression(None)
		self.data.parse(optelem.findall('*')[0])
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
	def parse(self,starelem):
		self.data = Expression(None)
		self.data.parse(starelem.findall('*')[0])
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
	def parse(self,pluselem):
		self.data = Expression(None)
		self.data.parse(pluselem.findall('*')[0])
	def setExpr(self,expr):
		self.data = expr
	def getXml(self):
		#print 'Getting the XML of +...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		ET.SubElement(self.ex,'plus').append(self.data.getXml())
		return self.ex
	def __str__(self):
		return str(self.data)+'+'


# sepliststar
class SepListStar:
	def __init__(self):
		self.item = None
		self.sep = None
	def parse(self,slelem):
		exprs = slelem.findall(slpsns.bgf_('expression'))
		self.item = Expression(None)
		self.item.parse(exprs[0])
		self.sep = Expression(None)
		self.sep.parse(exprs[1])
	def setItem(self,name):
		self.item = name
	def setSep(self,expr):
		self.sep = expr
	def getXml(self):
		#print 'Getting the XML of selectable',self.sel,'...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		self.xml = ET.SubElement(self.ex,'sepliststar')
		self.xml.append(self.item.getXml())
		self.xml.append(self.sep.getXml())
		return self.ex
	def __str__(self):
		return '{'+str(self.item)+' '+str(self.sep)+'}*'

# seplistplus
class SepListPlus:
	def __init__(self):
		self.item = None
		self.sep = None
	def parse(self,slelem):
		exprs = slelem.findall(slpsns.bgf_('expression'))
		self.item = Expression(None)
		self.item.parse(exprs[0])
		self.sep = Expression(None)
		self.sep.parse(exprs[1])
	def setItem(self,name):
		self.item = name
	def setSep(self,expr):
		self.sep = expr
	def getXml(self):
		#print 'Getting the XML of selectable',self.sel,'...'
		self.ex = ET.Element(slpsns.bgf_('expression'))
		self.xml = ET.SubElement(self.ex,'seplistplus')
		self.xml.append(self.item.getXml())
		self.xml.append(self.sep.getXml())
		return self.ex
	def __str__(self):
		return '{'+str(self.item)+' '+str(self.sep)+'}+'
