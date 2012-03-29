#!/usr/local/bin/python
import os,sys
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import slpsns
import xml.etree.ElementTree as ET

class AnyElement:
	def __init__(self,data,role):
		self.data = data
		self.role = role
	def setV(self,e):
		self.data = e
	def getXML(self):
		self.root = ET.Element(slpsns.rpl_(self.tag))
		self.root.text = self.data
		if self.role:
			self.root.attrib['role'] = self.role
		return self.root
	def boxwrap(self,x):
		if self.role:
			s = '<span class="box '+self.role+'">'+x+'</span>'
		else:
			s = '<span class="box">'+x+'</span>'
		return s
	def getHTML(self):
		return self.boxwrap(self.data)

class RPL:
	def __init__(self,name):
		self.name = name
		self.data = []
	def add(self,e):
		self.data.append(e)
	def getXML(self):
		self.root = ET.Element(slpsns.rpl_('sequence'))
		for a in self.data:
			self.root.append(a.getXML())
		return self.root
	def dump(self):
		ET.ElementTree(self.getXML()).write(self.name)
	def parse(self):
		self.xml = ET.parse(self.name)
		for e in self.xml.findall('*'):
			if e.tag == slpsns.rpl_('title'):
				self.data.append(Title(e.text))
			elif e.tag == slpsns.rpl_('message'):
				self.data.append(Message(e.text))
			elif e.tag == slpsns.rpl_('step'):
				self.data.append(Step(e.text))
			elif e.tag == slpsns.rpl_('tokens'):
				ts = TokenSeq()
				ts.parse(e)
				self.data.append(ts)
			else:
				print(e.tag)
	def getHTML(self):
		return  '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>'+\
				'<title>Grammar Recovery, visualised: '+self.name+'</title><link href="recovery.css" rel="stylesheet" type="text/css"/></head><body>'+\
				'\n'.join(map(lambda x:x.getHTML(),self.data))+\
				'<hr></body></html>'

class TokenSeq:
	def __init__(self):
		self.data = []
	def add(self,e):
		self.data.append(e)
	def prods2tokens(self,prods):
		for p in prods:
			pass
		#not implemented
	def parse(self,elem):
		for e in elem.findall('*'):
			if e.tag == slpsns.rpl_('token'):
				if 'role' in e.attrib:
					self.data.append(Token(e.text,e.attrib['role']))
				else:
					self.data.append(Token(e.text,''))
			elif e.tag == slpsns.rpl_('line'):
				self.data.append(Line(e.text))
			elif e.tag == slpsns.rpl_('tokens'):
				ts = TokenSeq()
				ts.parse(e)
				self.data.append(ts)
			else:
				print(e.tag)
	def getXML(self):
		self.root = ET.Element(slpsns.rpl_('tokens'))
		for a in self.data:
			self.root.append(a.getXML())
		return self.root
	def getHTML(self):
		return '<pre>'+self.getHTML2()+'</pre>'
	def getHTML2(self):
		s = ''
		for x in self.data:
			if x.__class__.__name__ == 'TokenSeq':
				s += ' '+ x.getHTML2()
			else:
				s += ' '+ x.getHTML()
		return s

class Token (AnyElement):
	def __init__(self,data,role):
		self.tag = 'token'
		super(Token,self).__init__(data,role)
	def getHTML(self):
		if self.data == '\n':
			return self.boxwrap('↩')+'\n'
		else:
			return self.boxwrap(self.data)

class Title (AnyElement):
	def __init__(self,data):
		self.tag = 'title'
		super(Title,self).__init__(data,'')
	def getHTML(self):
		return '<h1>'+self.data+'</h1>'

class Message (AnyElement):
	def __init__(self,data):
		self.tag = 'message'
		super(Message,self).__init__(data,'')
	def getHTML(self):
		return '<h3>'+self.data+'</h3>'

class Step (AnyElement):
	def __init__(self,data):
		self.tag = 'step'
		super(Step,self).__init__(data,'')
	def getHTML(self):
		return '<h2>Step '+self.data+'</h2>'

class Line (AnyElement):
	def __init__(self,data):
		self.tag = 'line'
		super(Line,self).__init__(data,'')
	def getHTML(self):
		return self.boxwrap(self.data)+'\n'
