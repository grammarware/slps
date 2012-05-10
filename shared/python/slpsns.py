#!/usr/bin/python

lcfns  = 'http://planet-sl.org/lcf'
ldfns  = 'http://planet-sl.org/ldf'
xldfns = 'http://planet-sl.org/xldf'
bgfns  = 'http://planet-sl.org/bgf'
xbgfns = 'http://planet-sl.org/xbgf'
cbgfns = 'http://planet-sl.org/cbgf'
mbgfns = 'http://planet-sl.org/mbgf'
eddns  = 'http://planet-sl.org/edd'
xeddns = 'http://planet-sl.org/xedd'
rplns  = 'http://planet-sl.org/rpl'
xsdns  = 'http://www.w3.org/2001/XMLSchema'
htmlns = 'http://www.w3.org/1999/xhtml'

def init(ET):
	ET._namespace_map[lcfns]  =  'lcf'
	ET._namespace_map[ldfns]  =  'ldf'
	ET._namespace_map[xldfns] = 'xldf'
	ET._namespace_map[bgfns]  =  'bgf'
	ET._namespace_map[xbgfns] = 'xbgf'
	ET._namespace_map[cbgfns] = 'cbgf'
	ET._namespace_map[mbgfns] = 'mbgf'
	ET._namespace_map[eddns]  =  'edd'
	ET._namespace_map[xeddns] = 'xedd'
	ET._namespace_map[rplns]  =  'rpl'
	ET._namespace_map[xsdns]  =  'xsd'
	ET._namespace_map[htmlns] = 'html'

def lcf_(x):  return '{'+lcfns+'}' +x
def ldf_(x):  return '{'+ldfns+'}' +x
def xldf_(x): return '{'+xldfns+'}'+x
def bgf_(x):  return '{'+bgfns+'}' +x
def xbgf_(x): return '{'+xbgfns+'}'+x
def cbgf_(x): return '{'+cbgfns+'}'+x
def mbgf_(x): return '{'+mbgfns+'}'+x
def xsd_(x):  return '{'+xsdns+'}' +x
def edd_(x):  return '{'+eddns+'}' +x
def xedd_(x): return '{'+xeddns+'}'+x
def rpl_(x):  return '{'+rplns+'}' +x
def htm_(x):  return '{'+htmlns+'}'+x

