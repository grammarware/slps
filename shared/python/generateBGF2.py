#!/usr/bin/python

import sys

maxdepth = 2

def generate_epsilon():
	return ['<bgf:expression><epsilon/></bgf:expression>']

def generate_nonterminal():
	return ['<bgf:expression><nonterminal>foo</nonterminal></bgf:expression>','<bgf:expression><nonterminal>bar</nonterminal></bgf:expression>']

def generate_terminal():
	return ['<bgf:expression><terminal>begin</terminal></bgf:expression>','<bgf:expression><terminal>end</terminal></bgf:expression>']

def generate_unary(name,depth):
	global maxdepth
	r = []
	for x in generate_nonterminal():
		r.append(x)
	for x in generate_terminal():
		r.append(x)
	if depth < maxdepth:
		for x in generate_expression(depth+1):
			r.append(x)
	return map(lambda x:'<bgf:expression><'+name+'>'+x+'</'+name+'></bgf:expression>',r)

def generate_binary(name,depth):
	global maxdepth
	p = []
	for x in generate_nonterminal():
		p.append(x)
	for x in generate_terminal():
		p.append(x)
	if depth < maxdepth:
		for x in generate_expression(depth+1):
			p.append(x)
	r = []
	for x in p:
		for y in p:
			r.append('<bgf:expression><'+name+'>'+x+y+'</'+name+'></bgf:expression>')
	return r

def generate_sequence(depth):
	return generate_binary('sequence',depth)

def generate_choice(depth):
	return generate_binary('choice',depth)

def generate_optional(depth):
	return generate_unary('optional',depth)

def generate_plus(depth):
	return generate_unary('plus',depth)

def generate_star(depth):
	return generate_unary('star',depth)

def generate_expression(depth):
	global maxdepth
	r = []
	#for x in generate_epsilon():
	#	r.append(x)
	for x in generate_nonterminal():
		r.append(x)
	for x in generate_terminal():
		r.append(x)
	if depth < maxdepth:
		for x in generate_expression(depth+1):
			r.append(x)
	return map(lambda x:'<bgf:expression><'+name+'>'+x+'</'+name+'></bgf:expression>',r)

#basic_elements = ['e','o','i','s','a','t','n']
#unary_modifiers = ['z','m','q','p','k']
binary_modifiers = ['f','c']
names_subsets = ([],['foo'],['foo','bar'])
counter = 0
label = True

def main(dir,basic_elements='eoisatn',unary_modifiers='zmqpk',options=''):
	global counter, label
	if options == '--nolabel':
		print 'No labels will be generated.'
		label = False
	# solitary basic elements
	for x in basic_elements:
		dump_file(generate_expression(x),dir+'/'+x+'.bgf')
	print counter,'test cases with basic elements generated successfully'
	counter = 0
	# once wrapped basic elements
	for x in unary_modifiers:
		for y in basic_elements:
			dump_file(wrap_expression(x,generate_expression(y)),dir+'/'+x+y+'.bgf')
	print counter,'test cases with wrapped basic elements generated successfully'
	counter = 0
	# twice wrapped basic elements
	for x in unary_modifiers:
		for y in unary_modifiers:
			for z in basic_elements:
				dump_file(wrap_expression(x,wrap_expression(y,generate_expression(z))),dir+'/'+x+y+z+'.bgf')
	print counter,'test cases with twice wrapped basic elements generated successfully'
	counter = 0
	# sequences and choices
	for x in binary_modifiers:
		for y in basic_elements:
			for z in basic_elements:
				dump_file(wrap_expression(x,generate_expression(y)+generate_expression(z)),dir+'/'+x+y+z+'.bgf')
	print counter,'test cases with sequences and choices of length 2 generated successfully'
	counter = 0
	# longer sequences and choices
	for x in binary_modifiers:
		for a in basic_elements:
			for b in basic_elements:
				for c in basic_elements:
					dump_file(wrap_expression(x,generate_expression(a)+generate_expression(b)+generate_expression(c)),dir+'/'+x+a+b+c+'.bgf')
	print counter,'test cases with sequences and choices of length 3 generated successfully'
	counter = 0
	# nested sequences and choices
	for x in binary_modifiers:
		for y in binary_modifiers:
			for a in basic_elements:
				for b in basic_elements:
					for c in basic_elements:
						dump_file(wrap_expression(x,wrap_expression(y,generate_expression(a)+generate_expression(b))+generate_expression(c)),dir+'/'+x+y+a+b+c+'.bgf')
						dump_file(wrap_expression(x,generate_expression(a)+wrap_expression(y,generate_expression(b)+generate_expression(c))),dir+'/'+x+a+y+b+c+'.bgf')
	print counter,'test cases with sequences and choices of nested depth 2 generated successfully'
	counter = 0
	# specific cases
	for x in names_subsets:
		for y in names_subsets:
			dump_specific_file(x,y,dir+'/r'+str(len(x))+'l'+str(len(y))+'.bgf')
	if label:
		print counter,'test cases with multiple labelled productions generated successfully'
	else:
		print counter,'test cases with multiple productions generated successfully'
	return

def dump_file(expr,fname):
	global counter, label
	f = open(fname,'w')
	f.write('<?xml version="1.0"?><bgf:grammar xmlns:bgf="http://planet-sl.org/bgf"><bgf:production>')
	if label:
		f.write('<label>foo</label>')
	f.write('<nonterminal>bar</nonterminal>')
	f.write(expr)
	f.write('</bgf:production></bgf:grammar>')
	f.close()
	counter += 1

def dump_specific_file(roots,labels,fname):
	global counter, label
	f = open(fname,'w')
	f.write('<?xml version="1.0"?><bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">')
	for r in roots:
		f.write('<root>'+r+'</root>')
	for l in labels:
		f.write('<bgf:production>')
		if label:
			f.write('<label>l'+l+'</label>	')
		f.write('<nonterminal>'+l+'</nonterminal>'+generate_expression('e')+'</bgf:production>')
	f.write('</bgf:grammar>')
	f.close()
	counter += 1

def generate_expression(code):
	# basic_elements = ['e','o','i','s','a','t','n']
	if code == 'e':
		expr = '<epsilon/>'
	elif code == 'o':
		expr = '<empty/>'
	elif code == 'i':
		expr = '<value>int</value>'
	elif code == 's':
		expr = '<value>string</value>'
	elif code == 'a':
		expr = '<any/>'
	elif code == 't':
		expr = '<terminal>x</terminal>'
	elif code == 'n':
		expr = '<nonterminal>x</nonterminal>'
	return '<bgf:expression>'+expr+'</bgf:expression>'

def wrap_expression(code,expr):
	# unary_modifiers = ['z','m','q','p','k']
	if code == 'z':
		expr = '<selectable><selector>x</selector>'+expr+'</selectable>'
	elif code == 'm':
		expr = '<marked>'+expr+'</marked>'
	elif code == 'q':
		expr = '<optional>'+expr+'</optional>'
	elif code == 'p':
		expr = '<plus>'+expr+'</plus>'
	elif code == 'k':
		expr = '<star>'+expr+'</star>'
	elif code == 'f':
		expr = '<sequence>'+expr+'</sequence>'
	elif code == 'c':
		expr = '<choice>'+expr+'</choice>'
	return '<bgf:expression>'+expr+'</bgf:expression>'

if __name__ == "__main__":
	if len(sys.argv) >= 2:
		apply(main,sys.argv[1:])
	else:
		print '''BGF Test Set Generator

Usage:'''
		print ' ',sys.argv[0],'<output directory>','[<basic elements>]','[<unary modifiers>]','[--nolabel]'
		sys.exit(1)
