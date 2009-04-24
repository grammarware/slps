#!/usr/bin/python
import elementtree.ElementTree as ET

lcfns = 'http://planet-sl.org/lcf'
ldfns = 'http://planet-sl.org/ldf'
xldfns = 'http://planet-sl.org/xldf'
bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
xsdns = 'http://www.w3.org/2001/XMLSchema'
htmlns= 'http://www.w3.org/1999/xhtml'

ET._namespace_map[lcfns] = 'lcf'
ET._namespace_map[ldfns] = 'ldf'
ET._namespace_map[xldfns] = 'xldf'
ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
ET._namespace_map[xsdns] = 'xsd'
ET._namespace_map[htmlns]='html'

def lcf_(x):
 return '{'+lcfns+'}'
def ldf_(x):
 return '{'+ldfns+'}'
def xldf_(x):
 return '{'+xldfns+'}'
def bgf_(x):
 return '{'+bgfns+'}'
def xbgf_(x):
 return '{'+xbgfns+'}'
def xsd_(x):
 return '{'+xsdns+'}'
def htm_(x):
 return '{'+htmlns+'}'
