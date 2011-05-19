#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os
import sys
import slpsns
import random
import elementtree.ElementTree as ET
import BGF

class NLGEngine:
	def __init__(self,grammar):
		self.g = grammar
	def describeGrammar(self):
		s = random.choice(('Here is a story about our grammar','Once upon a type there was a grammar'))
		s += '. '+self.describeRoot(self.g.roots) + '\n\n'+self.describeProduction(self.g.prods[0],True)+'\n\n'
		s += '\n\n'.join(map(lambda x:self.describeProduction(x,False),self.g.prods[1:]))
		return s.replace(' ,',',').replace(' .','.').replace('  ',' ').strip()
	def describeRoot(self,roots):
		if len(roots)==0:
			return 'There is no main nonterminal there. '
		elif len(roots)==1:
			return random.choice(('The main part of this grammar is '+self.describeNonterminal(roots[0]),'The grammar is ruled by '+self.describeNonterminal(roots[0])))+'. '
		else:
			return 'The grammar was ruled by '+', '.join(map(self.describeNonterminal,roots[:-1]))+'and '+self.describeNonterminal(roots[-1])+'. '
	def describeProduction(self,p,first):
		if p.label:
			if first:
				s = 'There is a rule called '
			else:
				s = random.choice(('There is also a rule called ','Another rule is called ','Consider a rule that goes by the name of ','Also, there is a rule titled '))
			s += p.label+'. It is about '+self.describeNonterminal(p.nt)+', which consist'
			if p.nt[-1] == 's':
				s += ' of '
			else:
				s += 's of '
		else:
			s = self.describeNonterminal(p.nt).capitalize()+' consists of '
		s += self.describeExpression(p.expr)
		return s.strip()+'. '
	def describeNonterminal(self,name):
		if name[-1].lower() == 's':
			s = ''
		elif name[0].lower() in ('a','i','e','o','u','j','y'):
			s = 'an '
		else:
			s = 'a '
		return s+name.replace('_',' ').lower()+' '
	def describeTerminal(self,name):
		btype = {'(':('parenthesis ','round bracket','open bracket'),
		')':('parenthesis ','open bracket','round bracket'),
		'[':('square bracket','closed bracket'),
		']':('square bracket','closed bracket'),
		'{':('curly bracket','swirly bracket','birdie bracket','squiggly bracket','fancy bracket'),
		'}':('curly bracket','swirly bracket','birdie bracket','squiggly bracket','fancy bracket'),
		'<':('angle bracket','triangular bracket','diamond bracket','chevron','fish bracket'),
		'>':('angle bracket','triangular bracket','diamond bracket','chevron','fish bracket')}
		if name == ',':
			return 'a comma '
		elif name == '.':
			return 'a dot '
		elif name == '*':
			return 'a star '
		elif name == '+':
			return 'a plus '
		elif name == '-':
			return random.choice(('a minus ','a dash '))
		elif name == ':':
			return 'a colon '
		elif name == '&':
			return 'an ampersand '
		elif name == '#':
			return random.choice(('a hash ','a sharp sign ','a number sign '))
		elif name == ';':
			return 'a semicolon '
		elif name == '~':
			return random.choice(('a tilde ','a wave dash ','a swung dash '))
		elif name == ':=':
			return 'an assignment sign '
		elif name == '<=':
			return 'a less than or equals to sign '
		elif name == '>=':
			return 'a greater than or equals to sign '
		elif name == '=':
			return 'an equals sign '
		elif name in ('(','[','{','<'):
			return 'a '+random.choice(('opening','left'))+' '+random.choice(btype[name])+' '
		elif name in (')',']','}','>'):
			return 'a '+random.choice(('closing','right'))+' '+random.choice(btype[name])+' '
		elif name.isalpha() and name == name.upper():
			return 'an uppercased keyword “'+name.lower()+'” '
		else:
			return 'a keyword “'+name+'” '
	def describeSequence(self,a):
		return 'followed by '.join(map(self.describeExpression,a))+' '
	def describeSelectable(self,name,desc):
		return 'something that we will call '+name+' but which is in fact '+desc+' '
	def describeEpsilon(self):
		return random.choice(('nothing','empty','an empty string','zilch'))+' '
	def describeAny(self):
		return random.choice(('anything','something unknown','some kind of string'))+' '
	def describeOptional(self,desc):
		return 'possibly '+desc
	def describeExpression(self,e):
		if e.wrapped.__class__.__name__ == 'Terminal':
			return self.describeTerminal(e.wrapped.data)
		elif e.wrapped.__class__.__name__ == 'Nonterminal':
			return self.describeNonterminal(e.wrapped.data)
		elif e.wrapped.__class__.__name__ == 'Sequence':
			return self.describeSequence(e.wrapped.data)
		elif e.wrapped.__class__.__name__ == 'Selectable':
			return self.describeSelectable(e.wrapped.sel,self.describeExpression(e.wrapped.expr))
		elif e.wrapped.__class__.__name__ == 'Optional':
			return self.describeOptional(self.describeExpression(e.wrapped.data))
		elif e.wrapped.__class__.__name__ == 'Epsilon':
			return self.describeEpsilon()
		elif e.wrapped.__class__.__name__ == 'Any':
			return self.describeAny()
		elif e.wrapped.__class__.__name__ == 'Empty':
			return self.describeEmpty()
		elif e.wrapped.__class__.__name__ == 'Plus':
			return 'one or more occurrences of '+self.describeExpression(e.wrapped.data)
		elif e.wrapped.__class__.__name__ == 'Star':
			return 'zero or more occurrences of '+self.describeExpression(e.wrapped.data)
		if True:
			return "I'm at a loss of words when I need to talk about "+e.wrapped.__class__.__name__+'! '
		elif e.wrapped.__class__.__name__ == 'Value':
			return self.describeValue(e.wrapped)
		elif e.wrapped.__class__.__name__ == 'Choice':
			return self.describeChoice(e.wrapped)
		
