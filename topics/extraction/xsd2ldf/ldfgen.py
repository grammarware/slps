#!/usr/bin/python
import sys
import elementtree.ElementTree as ET

ldfns = 'http://planet-sl.org/ldf'
bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
ldxns = 'http://planet-sl.org/ldx'
xsdns = 'http://www.w3.org/2001/XMLSchema'
htmlns= 'http://www.w3.org/1999/xhtml'

ET._namespace_map[ldfns] = 'ldf'
ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
ET._namespace_map[ldxns]='ldx'
ET._namespace_map[xsdns] = 'xsd'
ET._namespace_map[htmlns]='html'

def main(xsdfile,bgffile,ldffile):
 grammar={}
 gtree = ET.parse(bgffile)
 stree = ET.parse(xsdfile)
 dtree = ET.Element('ldf:document')

 cx=0
 for prod in gtree.findall('//{%s}production' % bgfns):
  cx+=1
  grammar[prod.findtext('nonterminal')]=prod
 print 'Found', cx, 'productions'

 dtree.set('xmlns:ldf',ldfns)
 dtree.set('xmlns:bgf',bgfns)
 dtree.set('xmlns:ldx',ldxns)
 dtree.set('xmlns:html',htmlns)

 el = copymixedcontent(dtree,'title',stree,'/{%s}annotation/{%s}documentation' % (xsdns,xsdns))
 el = ET.SubElement(dtree,'author')
 el.text = 'XSD2LDF generator'
 el = ET.SubElement(dtree,'abstract')
 el.text = '...abstract...'
 content = ET.SubElement(dtree,'content')

 for nt in stree.findall('/{%s}group' % (xsdns)):
  s = ET.SubElement(content,'section')
  el = ET.SubElement(s,'title')
  el.text = nt.attrib['name']
  con2 = ET.SubElement(s,'content')
  el = copymixedcontent(con2,'text',nt,'./{%s}annotation/{%s}documentation' % (xsdns,xsdns))
  el = ET.SubElement(con2,'grammar')
  el.set('language',xbgfns)
  el.append(grammar[nt.attrib['name']])
  #print 'found group!'

 for nt in stree.findall('/{%s}element' % (xsdns)):
  s = ET.SubElement(content,'section')
  el = ET.SubElement(s,'title')
  el.text = nt.attrib['name']
  con2 = ET.SubElement(s,'content')
  el = copymixedcontent(con2,'text',nt,'./{%s}annotation/{%s}documentation' % (xsdns,xsdns))
  el = ET.SubElement(con2,'grammar')
  el.set('language',xbgfns)
  el.append(grammar[nt.attrib['name']])

 ET.ElementTree(dtree).write(ldffile)

def bgfns_(element):
 return '{http://planet-sl.org/bgf}'+element

def copymixedcontent (parent, name, stree, xpath):
 element = ET.SubElement(parent, name)
 if not stree.findall(xpath):
  return None
 mixed = stree.findall(xpath)[0]
 if mixed.text:
  element.text=mixed.text
 for tag in mixed:
  element.append(tag)
 return element

if __name__ == "__main__":
 if len(sys.argv) == 4:
  apply(main,sys.argv[1:4])
 else:
  print '''This tool generates Language Definition Format documents from XML Schema Definitions,
also taking a BGF file as an input (probably strenghtening functionality of XSD2BGF up to XSD2LDF)

Usage:'''
  print ' ',sys.argv[0],'<input xsd file>','<input bgf file>','<output ldf file>'
  sys.exit(1)
