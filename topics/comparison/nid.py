#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
# -*- coding: utf-8 -*-
import os, sys
import xml.etree.ElementTree as ET
sys.path.append(os.getcwd().split('projects')[0]+'projects/slps/shared/python')
import BGF3
from functools import reduce
import ndd

hyp = {}

def isEqualG(g1,g2):
	# checks equality on grammars
	#print('isEqualG(%s,%s)' % (g1,g2))
	if areEqualS(g1.roots,g2.roots) and areEqualP(g1.prods,g2.prods):
		#print("--> YES")
		print("Checking...")
		ndd.loadMapping(hyp)
		if ndd.isEqualG(g1,g2):
			print('Hypothesis is correct.')
			return True
		else:
			print('Hypothesis is incorrect.')
			return False
		#return True
	else:
		#print("--isEqualG--> NO")
		return False

def areEqualS(l1,l2):
	# checks equality on lists of strings
	#print('areEqualS(%s,%s)' % (l1,l2))
	l3 = l2[:]
	for x in l1:
		if x in l3:
			l3.remove(x)
		else:
			#print("--> NO")
			return False
	#print("--areEqualS--> YES")
	return True

def areEqualP(l1,l2):
	# checks equality on lists of productions
	#print('areEqualP(%s,%s)' % (l1,l2))
	l3 = l2[:]
	for x in l1:
		for y in l3:
			if isEqualP(x,y):
				l3.remove(y)
				break
		else:
			#print("--areEqualP--> NO")
			return False
	#print("--areEqualP--> YES")
	return True

def areEqualE(l1,l2):
	# checks equality on lists of expressions
	#print('areEqualE(%s,%s)' % (l1,l2))
	l3 = l2[:]
	for x in l1:
		for y in l3:
			if isEqualE(x,y):
				l3.remove(y)
				break
		else:
			#print("--areEqualE--> NO (unmatched %s)"%x)
			return False
	#print("--areEqualE--> YES")
	return True

def areEqualAlt(l1,l2):
	# checks equality on alternatives
	#print('areEqualAlt(%s,%s)' % (l1,l2))
	l3 = l2[:]
	for x in l1:
		for y in l3:
			if isEqualE(x,y):
				l3.remove(y)
				break
		else:
			#print("--areEqualAlt--> NO")
			return False
	#print("--areEqualAlt--> YES")
	return True

def isEqualP(p1,p2):
	#print('isEqualP(%s,%s)' % (p1,p2))
	# checks equality on productions
	if p1.label == p2.label and isEqualE(p1.expr,p2.expr):
		#print('Productions of %s and %s match' % (p1,p2))
		#print("--isEqualP--> YES")
		print('Renaming hypothesis: %s -> %s'%(p1.nt,p2.nt))
		hyp[p1.nt] = p2.nt
		return True
	else:
		#print("--isEqualP--> NO")
		return False

def isEqualE(e1,e2):
	# checks equality on expressions
	#print('isEqualE(%s,%s)' % (e1,e2))
	if str(e1.wrapped.__class__) == "<class 'BGF3.Sequence'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Sequence'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return areEqualE(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Choice'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Choice'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return areEqualAlt(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Optional'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Optional'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return isEqualE(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Star'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Star'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return isEqualE(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Plus'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Plus'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return isEqualE(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Nonterminal'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Nonterminal'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return isEqualNT(e1.wrapped.data,e2.wrapped.data)
	if str(e1.wrapped.__class__) == "<class 'BGF3.Terminal'>":
		if str(e2.wrapped.__class__) != "<class 'BGF3.Terminal'>":
			#print("--isEqualE--> NO (type mismatch)")
			return False
		return e1.wrapped.data == e2.wrapped.data
	print(str(e1.wrapped.__class__))
	#print("--isEqualE--> NO")
	return False

def isEqualNT(n1,n2):
	# all NTs are equal
	#print('isEqualNT(%s,%s)' % (n1,n2))
	#print("--isEqualNT--> YES")
	return True

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print('Usage:')
		print('	      name-independent-differ.py input1.bgf input2.bgf')
		sys.exit(-1)
	g1 = BGF3.Grammar()
	g1.parse(sys.argv[1])
	g2 = BGF3.Grammar()
	g2.parse(sys.argv[2])
	if isEqualG(g1,g2):
		sys.exit(0)
	else:
		sys.exit(-1)
