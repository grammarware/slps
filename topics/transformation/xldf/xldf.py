#!/usr/bin/python
import sys
import string
import elementtree.ElementTree as ET

ldfns = 'http://planet-sl.org/ldf'
xldfns = 'http://planet-sl.org/xldf'
bgfns = 'http://planet-sl.org/bgf'
xbgfns= 'http://planet-sl.org/xbgf'
#ldxns = 'http://planet-sl.org/ldx'
xsdns = 'http://www.w3.org/2001/XMLSchema'
htmlns= 'http://www.w3.org/1999/xhtml'

ET._namespace_map[ldfns] = 'ldf'
ET._namespace_map[xldfns] = 'xldf'
ET._namespace_map[bgfns] = 'bgf'
ET._namespace_map[xbgfns]='xbgf'
#ET._namespace_map[ldxns] = 'ldx'
ET._namespace_map[xsdns] = 'xsd'
ET._namespace_map[htmlns]='html'

acceptedtags = ('{'+xsdns+'}complexType','{'+xsdns+'}element','{'+xsdns+'}simpleType','{'+xsdns+'}group')

def preparetext(s):
 if not s:
  return s
 else:
  return ' '.join(map(string.strip,s.split('\n')))

def xldf_insert(cmd,tree):
 welookfor = preparetext(cmd.findtext('*/*'))
 for cnt in tree.findall('.//content'):
  passed = False
  for eli in range(0,len(cnt)):
   if preparetext(cnt[eli].text) == welookfor:
    cnt_idx = eli
    passed = True
  if passed:
   if cmd.findall('*')[0].tag=='after':
    cnt_idx += 1
   print '[XLDF] insert(',
   cx = 0
   ctag = ''
   for el in cmd.findall('content/*'):
    if cx:
     if ctag==el.tag:
      cx += 1
     else:
      if cx==1:
       print ctag,
      else:
       print ctag,'*',cx,
      cx = 1
      ctag = el.tag
    else:
     cx = 1
     ctag = el.tag
    #print el.tag,
    cnt.insert(cnt_idx,ET.Element(el.tag,{}))
    cnt[cnt_idx].text = el.text
    cnt_idx += 1
   if cx:
    if cx==1:
     print ctag,
    else:
     print ctag,'*',cx,
   print ')'
   return
 print '[----] xldf:insert failed, cannot find the designated target!'
 return

def findnode(tree,id):
 for s in tree.findall('//*'):
  if s.findall('id'):
   if s.findtext('id')==id:
    return s
 return None

def xldf_append(cmd,tree):
 found = findnode(tree,cmd.findtext('where'))
 if not found:
  print '[----] xldf:append failed: target id',cmd.findtext('where'),'not found'
 else:
  print '[XLDF] append(',cmd.findtext('where'),',',
  cx = 0
  ctag = ''
  for p in cmd.findall('content/*'):
   if cx:
    if ctag==p.tag:
     cx += 1
    else:
     if cx==1:
      print ctag.replace('{'+bgfns+'}',''),
     else:
      print ctag.replace('{'+bgfns+'}',''),'*',cx,
     cx = 1
     ctag = p.tag
   else:
    cx = 1
    ctag = p.tag
   #print found[-1].tag,found[-1][-1].tag
   if found[-1][-1].tag != 'content':
    found[-1].append(p)
    #found[-1].append(ET.Element('content',{}))
   else:
    found[-1][-1].append(p)
  if cx:
   if cx==1:
    print ctag.replace('{'+bgfns+'}',''),
   else:
    print ctag.replace('{'+bgfns+'}',''),'*',cx,
  print ')'
 return

def xldf_place(cmd,tree):
 #found = tree.findall('//core[id="'+cmd.findtext('./section')+'"]')
 found = findnode(tree,cmd.findtext('section'))
 if not found:
  print '[----] xldf:place failed: source node not found!'
  return
 #found2 = tree.findall('//core[id="'+cmd.findtext('./inside')+'"]')
 found2 = findnode(tree,cmd.findtext('inside'))
 if not found2:
  print '[----] xldf:place failed: target node not found!'
  return
 if found2.tag=='core':
  found.tag = 'subtopic'
  found2.append(found)
  tree.getroot().remove(found)
  print '[XLDF] place('+cmd.findtext('section')+',',cmd.findtext('inside')+')'
 elif found2.tag in ('definitions','abbreviations','languageOverview'):
  el = ET.SubElement(found2,'term')
  el2 = ET.SubElement(el,'name')
  el2.text = found.findtext('title')
  el2 = ET.SubElement(el,'definition')
  for el in found.findall('.//content/*'):
   el2.append(el)
  tree.getroot().remove(found)
  print '[XLDF] place('+cmd.findtext('section')+',',cmd.findtext('inside')+')'
 else:
  print '[----] xldf:place failed: don''t know how to place subsections in',found2.tag
 return

def xldf_rename(cmd,tree):
 if cmd.findall('from/title'):
  byid = False
  welookfor = cmd.findtext('from/title')
 else:
  byid = True
  welookfor = cmd.findtext('from/id')
 found = False
 for core in tree.findall('//core'):
  if not byid and welookfor == core.findtext('title'):
   core.findall('title')[0].text = cmd.findtext('to')
   found = True
  if byid and welookfor == core.findtext('id'):
   core.findall('title')[0].text = cmd.findtext('to')
   found = True
 if not found:
  print '[----] xldf:rename failed:',
  if byid:
   print 'id','"'+welookfor+'"',
  else:
   print 'title','"'+welookfor+'"',
  print 'not found!'
 else:
  print '[XLDF] rename('+welookfor,',',cmd.findtext('to')+')'
 return

def xldf_add_section(cmd,tree):
 success = False
 s = cmd.findall('*')[0]
 if s.tag in ('foreword','designGoals','scope','conformance','compliance','compatibility','notation','normativeReferences','documentStructure','whatsnew'):
  tree.findall('//frontMatter')[0].append(s)
  print '[XLDF] add-section to front matter'
  success = True
 elif s.tag in ('definitions','abbreviations','languageOverview'):
  if tree.findall('//lists'):
   tree.findall('//lists')[0].append(s)
  else:
   el = ET.Element('lists',{})
   el.append(s)
   for i in range(0,len(tree.findall('*'))):
    if tree.getroot()[i].tag=='frontMatter':
     tree.getroot().insert(i+1,el)
  print '[XLDF] add-section to lists'
  success = True
 elif s.tag in ('lineContinuations','whitespace','tokens','preprocessor','literals','lexical'):
  tree.findall('//lexicalPart')[0].append(s)
  print '[XLDF] add-section to lexical part'
  success = True
 elif s.tag == 'core':
  tree.getroot().append(s)
  print '[XLDF] add-section to the core'
  success = True
 if not success:
  print '[----] add-section failed'
 return

def xldf_import_grammar(cmd,tree):
 try:
  gtree = ET.parse(cmd.findtext('file'))
 except IOError,e:
  print '[----] xldf:import failed: file',cmd.findtext('file'),'not found'
  return
 found = findnode(tree,cmd.findtext('target'))
 if not found:
  print '[----] xldf:import failed: target id',cmd.findtext('where'),'not found'
 else:
  cx = 0
  for p in gtree.findall('{'+bgfns+'}production'):
   if found[-1][-1].tag != 'content':
    found[-1].append(p)
   else:
    found[-1][-1].append(p)
   cx += 1
  if cx:
   print '[XLDF] import(',cmd.findtext('target'),',',cmd.findtext('file'),')','-',cx,'productions'
  else:
   print '[----] xldf:import failed: no productions found in',cmd.findtext('file')
 return

def xldf_import_sample(cmd,tree):
 try:
  sample = open(cmd.findtext('file'),'r')
 except IOError,e:
  print '[----] xldf:import failed: file',cmd.findtext('file'),'not found'
  return
 found = findnode(tree,cmd.findtext('target'))
 if not found:
  print '[----] xldf:import failed: target id',cmd.findtext('where'),'not found'
 else:
  el = ET.Element('sample',{})
  el.text = ''.join(sample.readlines())
  if found[-1][-1].tag != 'content':
   found[-1].append(el)
  else:
   found[-1][-1].append(el)
  print '[XLDF] import(',cmd.findtext('target'),',',cmd.findtext('file'),')'
 return

def main(xldffile,inldffile,outldffile):
 grammar={}
 xtree = ET.parse(xldffile)
 ltree = ET.parse(inldffile)

 for cmd in xtree.findall('*'):#'//{%s}*'%xldfns):
  cmdname = cmd.tag.replace('{'+xldfns+'}','')
  if cmdname == 'insert':
   xldf_insert(cmd,ltree)
  elif cmdname == 'import-grammar':
   xldf_import_grammar(cmd,ltree)
  elif cmdname == 'import-sample':
   xldf_import_sample(cmd,ltree)
  elif cmdname == 'place':
   xldf_place(cmd,ltree)
  elif cmdname == 'rename':
   xldf_rename(cmd,ltree)
  elif cmdname == 'append':
   xldf_append(cmd,ltree)
  elif cmdname == 'add-section':
   xldf_add_section(cmd,ltree)
  else:
   print '[----] Unknown XLDF command:',cmdname

 ltree.write(outldffile)
 return

 cx=0
 for prod in gtree.findall('//{%s}production' % bgfns):
  cx+=1
  grammar[prod.findtext('nonterminal')]=prod
 print 'Found', cx, 'productions'

 dtree.set('xmlns:ldf',ldfns)
 dtree.set('xmlns:bgf',bgfns)
 #dtree.set('xmlns:ldx',ldxns)
 dtree.set('xmlns:html',htmlns)

 section = ET.SubElement(dtree,'titlePage')
 el = ET.SubElement(section,'author')
 el.text = 'XSD2LDF generator'
 el = ET.SubElement(section,'topic')
 el.text = stree.findall('/{%s}annotation/{%s}documentation' % (xsdns,xsdns))[0].text
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
 for p in stree.findall('/{%s}annotation/{%s}documentation' % (xsdns,xsdns))[1:]:
  pel = ET.SubElement(el,'p')
  pel.text = p.text

 if stree.findall('/{'+xsdns+'}import'):
  el = ET.SubElement(section,'normativeReferences')
  el = ET.SubElement(el,'content')
  el = ET.SubElement(el,'list')
  for p in stree.findall('/{'+xsdns+'}import'):
   pel = ET.SubElement(el,'item')
   pel.text = p.attrib['schemaLocation']

 #el = copymixedcontent(dtree,'title',stree,'/{%s}annotation/{%s}documentation' % (xsdns,xsdns))
 #el = ET.SubElement(dtree,'author')
 #el.text = 'XSD2LDF generator'
 #el = ET.SubElement(dtree,'abstract')
 #el.text = '...abstract...'
 #content = ET.SubElement(dtree,'content')

 for nt in stree.findall('/*'):
  if nt.tag not in acceptedtags:
   continue
  section = ET.SubElement(dtree,'core')
  el = ET.SubElement(section,'id')
  el.text = nt.tag.replace('{'+xsdns+'}','')+'-'+nt.attrib['name']
  el = ET.SubElement(section,'title')
  el.text = nt.attrib['name']
  el = ET.SubElement(section,'description')
  el = ET.SubElement(el,'content')
  for p in nt.findall('./{%s}annotation/{%s}documentation' % (xsdns,xsdns)):
   pel = ET.SubElement(el,'p')
   pel.text = p.text
  section.append(grammar[nt.attrib['name']])
  # print grammar[nt.attrib['name']]

 ET.ElementTree(dtree).write(ldffile)
 return

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
 return '{'+bgfns+'}'+element

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
  print '''LDF transformation engine

Usage:'''
  print ' ',sys.argv[0],'<input xldf file>','<input ldf file>','<output ldf file>'
  sys.exit(1)
