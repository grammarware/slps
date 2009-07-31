#!/usr/bin/python
import sys
import slpsns
import elementtree.ElementTree as ET

acceptedtags = (slpsns.xsd_('complexType'),slpsns.xsd_('element'),slpsns.xsd_('simpleType'),slpsns.xsd_('group'))

def mapXSD2LDF(stree,dtree,grammar):
 for nt in stree.findall('/*'):
  if nt.tag not in acceptedtags:
   continue
  section = ET.SubElement(dtree,'core')
  #section.set('id',nt.tag.replace(slpsns.xsd_(''),'')+'-'+nt.attrib['name'])
  # Dunno if it is a wise decision: replaced the "element-inline" naming scheme with just "inline". Probably will lead to name conflicts
  section.set('id',nt.attrib['name'])
  #el = ET.SubElement(section,'id')
  #el.text = nt.tag.replace('{'+xsdns+'}','')+'-'+nt.attrib['name']
  el = ET.SubElement(section,'title')
  el.text = nt.attrib['name']
  if nt.findall(slpsns.xsd_('annotation')):
   el = ET.SubElement(section,'synopsis')
   el = ET.SubElement(el,'content')
   #for p in nt.findall('.//{%s}annotation/{%s}documentation' % (xsdns,xsdns)):
   for p in nt.findall(slpsns.xsd_('annotation')+'/'+slpsns.xsd_('documentation')):
    pel = ET.SubElement(el,'text')
    pel.text = p.text
    # e.g. keywords
    for sub in p:
     pel.append(sub)
  gotit = []
  el = ET.SubElement(section,'syntax')
  el = ET.SubElement(el,'content')
  if nt.findall(slpsns.xsd_('choice')):
   #print 'Found a top level choice in',nt.get('name')
   for alt in nt.findall(slpsns.xsd_('choice')+'/*'):
    if alt.findall(slpsns.xsd_('annotation')+'/'+slpsns.xsd_('documentation')):
     name = alt.get('name')
     if not name:
      name = alt.get('ref').split(':')[-1]
     #print name,'got text!'
     for p in alt.findall(slpsns.xsd_('annotation')+'/'+slpsns.xsd_('documentation')):
      pel = ET.SubElement(el,'text')
      pel.text = p.text
      # e.g. keywords
      for sub in p:
       pel.append(sub)
     for prod in grammar[nt.get('name')]:
      if prod.findall('label'):
       #print 'Checking up on',prod.findtext('label'),'vs',name
       if prod.findtext('label') == name:
        #print 'Got it!'
        el.append(prod)
        gotit.append(prod)
  # Need to decide whether to put productions inside description subsections
  #section.append(grammar[nt.attrib['name']])
  for prod in grammar[nt.attrib['name']]:
   if prod not in gotit:
    el.append(prod)
  # print grammar[nt.attrib['name']]
	

def main(xsdfile,bgffile,ldffile):
 grammar={}
 gtree = ET.parse(bgffile)
 stree = ET.parse(xsdfile)
 dtree = ET.Element('ldf:document')

 cx=0
 for prod in gtree.findall('//'+slpsns.bgf_('production')):
  cx+=1
  nt = prod.findtext('nonterminal')
  if nt not in grammar.keys():
   grammar[nt] = []
  grammar[nt].append(prod)
 print 'Found', cx, 'productions'

 dtree.set('xmlns:ldf',slpsns.ldfns)
 dtree.set('xmlns:bgf',slpsns.bgfns)
 dtree.set('xmlns:html',slpsns.htmlns)

 section = ET.SubElement(dtree,'titlePage')
 el = ET.SubElement(section,'author')
 el.text = 'XSD2LDF generator'
 el = ET.SubElement(section,'topic')
 el.text = stree.findall('/'+slpsns.xsd_('annotation')+'/'+slpsns.xsd_('documentation'))[0].text
 el = ET.SubElement(section,'version')
 el.text = '1.0'
 el = ET.SubElement(section,'status')
 el.text = 'unknown'
 el = ET.SubElement(section,'date')
 # generate!!!
 el.text = '2008-02-21'

 section = ET.SubElement(dtree,'frontMatter')
 el = ET.SubElement(section,'foreword')
 el = ET.SubElement(el,'content')
 for p in stree.findall('/'+slpsns.xsd_('annotation')+'/'+slpsns.xsd_('documentation'))[1:]:
  pel = ET.SubElement(el,'text')
  pel.text = p.text
 if not el.findall('*'):
  ET.SubElement(el,'empty')

 if stree.findall('/'+slpsns.xsd_('import')):
  el = ET.SubElement(section,'normativeReferences')
  el = ET.SubElement(el,'content')
  el = ET.SubElement(el,'list')
  for p in stree.findall('/'+slpsns.xsd_('import')):
   pel = ET.SubElement(el,'item')
   pel.text = p.attrib['schemaLocation']
   istree = ET.parse('/'.join(xsdfile.split('/')[:-1])+'/'+p.attrib['schemaLocation'])
   mapXSD2LDF(istree,dtree,grammar)
  print len(stree.findall('/'+slpsns.xsd_('import'))),'external schema(ta) imported.'

 mapXSD2LDF(stree,dtree,grammar)

 ET.ElementTree(dtree).write(ldffile)
 return

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
  slpsns.init(ET)
  apply(main,sys.argv[1:4])
 else:
  print '''This tool generates Language Definition Format documents from XML Schema Definitions,
also taking a BGF file as an input (probably strenghtening functionality of XSD2BGF up to XSD2LDF)

Usage:'''
  print ' ',sys.argv[0],'<input xsd file>','<input bgf file>','<output ldf file>'
  sys.exit(1)
