#!/usr/bin/python

lcfns = 'http://planet-sl.org/lcf'
ldfns = 'http://planet-sl.org/ldf'
xldfns = 'http://planet-sl.org/xldf'
bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
xsdns = 'http://www.w3.org/2001/XMLSchema'
htmlns= 'http://www.w3.org/1999/xhtml'

def init(ET):
 ET._namespace_map[lcfns] = 'lcf'
 ET._namespace_map[ldfns] = 'ldf'
 ET._namespace_map[xldfns] = 'xldf'
 ET._namespace_map[bgfns] = 'bgf'
 ET._namespace_map[xbgfns]='xbgf'
 ET._namespace_map[xsdns] = 'xsd'
 ET._namespace_map[htmlns]='html'

def lcf_(x):
 return '{'+lcfns+'}'+x
def ldf_(x):
 return '{'+ldfns+'}'+x
def xldf_(x):
 return '{'+xldfns+'}'+x
def bgf_(x):
 return '{'+bgfns+'}'+x
def xbgf_(x):
 return '{'+xbgfns+'}'+x
def xsd_(x):
 return '{'+xsdns+'}'+x
def htm_(x):
 return '{'+htmlns+'}'+x
