#!/usr/bin/python
import os
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
 if cmd.findall('*')[0].findall('*')[0].tag != 'text':
  xldf_insert_symbolic(cmd,tree)
  return
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
    for k in el:
     cnt[cnt_idx].append(k)
    #print el.tail
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

def xml_eq(x,y):
 if preparetext(x.text)!=preparetext(y.text):
  return False
 ex = x.findall('*')
 ey = y.findall('*')
 if len(ex)!=len(ey):
  return False
 for i in range(0,len(ex)):
  if not xml_eq(ex[i],ey[i]):
   return False
 return True

def xldf_insert_symbolic(cmd,tree):
 welookfor = cmd.findall('*')[0].findall('*')[0]
 for cnt in tree.findall('.//content'):
  passed = False
  for eli in range(0,len(cnt)):
   if xml_eq(cnt[eli],welookfor):
    cnt_idx = eli
    passed = True
  if passed:
   if cmd.findall('*')[0].tag=='after':
    cnt_idx += 1
   print '[XLDF] insertS(',
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
    for k in el:
     cnt[cnt_idx].append(k)
    #print el.tail
    cnt_idx += 1
   if cx:
    if cx==1:
     print ctag,
    else:
     print ctag,'*',cx,
   print ')'
   return
 print '[----] xldf:insertS failed, cannot find the designated target!'
 return

def findnode(tree,id):
 for s in tree.findall('//*'):
  if s.get('id'):
   if s.get('id')==id:
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

def xldf_drop(cmd,tree):
 found = findnode(tree,cmd.findtext('section'))
 if not found:
  print '[----] xldf:drop failed: node',cmd.findtext('section'),'not found!'
  return
 tree.getroot().remove(found)
 print '[XLDF] drop('+cmd.findtext('section')+')'
 return

def xldf_combine(cmd,tree):
 found = findnode(tree,cmd.findtext('section'))
 if not found:
  print '[----] xldf:combine failed: source node not found!'
  return
 found2 = findnode(tree,cmd.findtext('with'))
 if not found2:
  print '[----] xldf:combine failed: target node not found!'
  return
 target = found2.findall('content')[0]
 for p in found.findall('*/content/*'):
  target.append(p)
 tree.getroot().remove(found)
 print '[XLDF] combine('+cmd.findtext('section')+',',cmd.findtext('with')+')'
 #else:
 # print '[----] xldf:combine failed: don''t know how to place subsections in',found2.tag
 return

def xldf_retitle(cmd,tree):
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
 if s.tag in ('definitions','abbreviations','languageOverview'):
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
 elif s.tag in ('core','annex'):
  tree.getroot().append(s)
  print '[XLDF] add-section to the',s.tag
  success = True
 elif s.tag == 'placeholder':
  tree.getroot().insert(1,s)
  print '[XLDF] add-section to placeholders'
  success = True
 if not success:
  print '[----] xldf:add-section failed, double check or try add-subsection instead'
 return

def xldf_remove_section(cmd,tree):
 found = findnode(tree,cmd.findtext('id'))
 if found:
  if cmd.findall('from'):
   foundp = findnode(tree,cmd.findtext('from'))
   foundp.remove(found)
  else:
   tree.getroot().remove(found)
  print '[XLDF] remove-section is successful'
 else:
  print '[----] xldf:remove-section couldn''t find id',cmd.findtext('id')

def xldf_add_subsection(cmd,tree):
 success = False
 s = cmd.findall('*')[0]
 if s.tag in ('foreword','designGoals','scope','conformance','compliance','compatibility','notation','normativeReferences','documentStructure','whatsnew'):
  tree.findall('//frontMatter')[0].append(s)
  print '[XLDF] add-subsection (',s.tag,', front matter, ...)'
  success = True
 elif s.tag in ('purpose','description','location','considerations','defaults','normative','note','example','informative'):
  found = findnode(tree,cmd.findtext('to'))
  if found:
   found.append(s)
   print '[XLDF] add-subsection (',s.tag,',',cmd.findtext('to'),'...)'
   success = True
  else:
   print '[----] xldf:add-subsection failed, can''t find id',cmd.findtext('to')
   return
 if not success:
  print '[----] xldf:add-subsection failed, double check or try add-section instead'
 return

def xldf_extract_subsection(cmd,tree):
 where = findnode(tree,cmd.findtext('from'))
 if not where:
  print '[----] xldf:extract-subsection failed, can''t find id',cmd.findtext('to')
  return
 for e1 in where.findall('*/content/*'):
  for e2 in cmd.findall('content/*'):
   if xml_eq(e1,e2):
    where.findall('*/content')[0].remove(e1)
 st = ET.SubElement(where,'subtopic')
 e = ET.SubElement(st,'title')
 e.text = cmd.findtext('title')
 e = ET.SubElement(st,'description')
 e = ET.SubElement(e,'content')
 for e2 in cmd.findall('content/*'):
  e.append(e2)
 print '[XLDF] extract-subsection (',cmd.findtext('from'),', content,',cmd.findtext('title'),
 if cmd.findall('id'):
  st.set('id',cmd.findtext('id'))
  print ',',cmd.findtext('id'),
 print ')'
 return

def xldf_add_figure(cmd,tree):
 success = False
 s = cmd.findall('*')[0]
 found = findnode(tree,cmd.findtext('to'))
 if found:
  if found[-1].tag=='content':
   found[-1].append(s)
   print '[XLDF] add-figure to id',cmd.findtext('to')
   success = True
 else:
  print '[----] add-figure failed, can''t find id',cmd.findtext('to')
  return
 if not success:
  print '[----] add-figure failed, double check or try add-section instead'
 return

def xldf_transform_grammar(cmd,tree):
 root = ET.Element('{'+xbgfns+'}sequence',{})
 cx0 = 0
 for rule in cmd.findall('*')[1:]:
  if rule.tag != 'context':
   root.append(rule)
   cx0 += 1
 ET.ElementTree(root).write('xldf-tmp.xbgf')
 found = findnode(tree,cmd.findtext('target'))
 if not found:
  print '[----] xldf:transform failed: target id',cmd.findtext('where'),'not found'
  return
 realprods = []
 contextprods = []
 for p in found.findall('*/*/{'+bgfns+'}production'):
  realprods.append(p)
 for p in found.findall('*/{'+bgfns+'}production'):
  realprods.append(p)
 for c in cmd.findall('context'):
  f = findnode(tree,c.text)
  if not f:
   print '[----] xldf:transform failed: context target id',c.text,'not found'
   return
  for p in f.findall('*/*/{'+bgfns+'}production'):
   contextprods.append(p)
 root = ET.Element('{'+bgfns+'}grammar',{})
 for p in realprods:
  root.append(p)
 for p in contextprods:
  root.append(p)
 #print '[====]',len(realprods),'+',len(contextprods),'productions'
 ET.ElementTree(root).write('xldf-tmp.bgf')
 #found.findall('{'+bgfns+'}production')[0].write('xldf-tmp.bgf')
 if os.system('xbgf xldf-tmp.xbgf xldf-tmp.bgf xldf-tmp-result.bgf | grep -v Loading | grep -v Saving'):
  print '[----] xldf:transform failed: error in XBGF'
  return
 try:
  gtree = ET.parse('xldf-tmp-result.bgf')
 except IOError,e:
  print '[----] xldf:transform failed: XBGF result file not found'
  sys.exit(3)
  return
 # remove old production
 cx1 = 0
 if found[-1][-1].tag != 'content':
  for p in found[-1].findall('{'+bgfns+'}production'):
   found[-1].remove(p)
   cx1 +=1
 else:
  for p in found[-1][-1].findall('{'+bgfns+'}production'):
   found[-1][-1].remove(p)
   cx1 +=1
 # add new productions
 cx2 = 0
 for p in gtree.findall('{'+bgfns+'}production'):
  isContext = False
  for cp in contextprods:
   if xml_eq(cp,p):
    isContext = True
  if isContext:
   continue
  if found[-1][-1].tag != 'content':
   found[-1].append(p)
  else:
   found[-1][-1].append(p)
  cx2 += 1
 if cx2:
  print '[XLDF] transform(',cmd.findtext('target'),', ...)','-',cx0,'x-rules,',cx1,':',cx2,'productions'
 else:
  print '[----] xldf:transform failed: no productions found in XBGF output'
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
   print '[XLDF] import(',cmd.findtext('target'),',',cmd.findtext('file').split('/')[-1],')','-',cx,'productions'
  else:
   print '[----] xldf:import failed: no productions found in',cmd.findtext('file')
 return

def xldf_import_sample(cmd,tree):
 ending = ''
 if cmd.findall('prettyprinter'):
  inputfile = 'printed_for_xldf.tmp'
  if os.system(cmd.findtext('prettyprinter')+' '+cmd.findtext('file')+' '+inputfile):
   print '[----] xldf:import failed: can''t execute the pretty-printer!'
   return
  ending = '- pretty-printed successfully'
 else:
  inputfile = cmd.findtext('file')
 try:
  sample = open(inputfile,'r')
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
  print '[XLDF] import(',cmd.findtext('target'),',',cmd.findtext('file').split('/')[-1],')',ending
 return

def xldf_transform_document(cmd,tree):
 print '[XLDF] transform(...,',cmd.findtext('file').split('/')[-1],') starts'
 try:
  xtree = ET.parse(cmd.findtext('file'))
 except IOError,e:
  print '[----] xldf:transform failed: file',cmd.findtext('file'),'not found'
  return
 for incmd in xtree.findall('*'):
  xldf_perform_command(incmd,tree)
 print '[XLDF] transform(...,',cmd.findtext('file').split('/')[-1],') is done'
 return

def xldf_perform_command(cmd,ltree):
 try:
  eval('xldf_'+cmd.tag.replace('{'+xldfns+'}','').replace('-','_'))(cmd,ltree)
 except NameError,e:
  print '[----] Unknown XLDF command:',cmd.tag.replace('{'+xldfns+'}','')

def main(xldffile,inldffile,outldffile):
 grammar={}
 xtree = ET.parse(xldffile)
 ltree = ET.parse(inldffile)
 for cmd in xtree.findall('*'):
  xldf_perform_command(cmd,ltree)
 ltree.write(outldffile)
 return

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
