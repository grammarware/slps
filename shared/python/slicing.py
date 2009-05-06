#!/usr/bin/python
import elementtree.ElementTree as ET
import slpsns

def sliceFile(xbgfDir1,xbgfDir2,text):
 sliced = []
 xtree = ET.parse(xbgfDir1+text+'.xbgf')
 cx = 0
 for t in xtree.findall('*'):
  cx += 1
  seq = ET.Element(slpsns.xbgf_('sequence'))
  if t.tag==slpsns.xbgf_('atomic'):
   for sub in t.findall('*'):
    seq.append(sub)
  else:
   seq.append(t)
  ET.ElementTree(seq).write(xbgfDir2+text+'-'+`cx`+'.xbgf')
  sliced.append(text+'-'+`cx`)
 return sliced

def sliceTarget(xbgfDir1,xbgfDir2,t):
 print 'Splitting target',t.findtext('name'),'...'
 nt = ET.Element('target')
 nt.append(t.findall('name')[0])
 for branch in t.findall('branch'):
  needNormalizing = False
  normXbgf = None
  normFiles = ''
  nbr = ET.Element('branch')
  for e in branch.findall('*'):
   if e.tag == 'input':
    nbr.append(e)
   elif e.tag in ('preparation','nominal-matching','normalizing'):
    needNormalizing = True
    for step in e.findall('*'):
     if step.tag=='perform':
      normXbgf = appendXbgf(normXbgf,xbgfDir1+step.text+'.xbgf')
      normFiles += step.text+' & '
     else:
      normXbgf = appendXbgf(normXbgf,xbgfDir1+step.findtext('result')+'.xbgf')
      normFiles += step.findtext('result')+' & '
   else:
    if needNormalizing:
     phase = ET.SubElement(nbr,'normalizing')
     p = ET.SubElement(phase,'perform')
     p.text = 'normalize-'+nbr.findtext('input')+'-'+nt.findtext('name')
     needNormalizing = False
     ET.ElementTree(normXbgf).write(xbgfDir2+p.text+'.xbgf')
     print '-->',normFiles[:-3],'accumulated to normalisation'
     normFiles = ''
    phase = ET.Element(e.tag)
    for step in e.findall('*'):
     # perform or automated
     print '-->',step.tag,
     if step.tag=='perform':
      print step.text,'-',
      slices = sliceFile(xbgfDir1,xbgfDir2,step.text)
      print len(slices),'slices'
      for s in slices:
       p = ET.Element('perform')
       p.text = s
       phase.append(p)
     else:
      print step.findtext('result'),'-',
      slices = sliceFile(xbgfDir1,xbgfDir2,step.findtext('result'))
      print len(slices),'slices'
      for s in slices:
       p = ET.Element('perform')
       p.text = s
       phase.append(p)
     #phase.append(step)
    nbr.append(phase)
  nt.append(nbr)
 return nt

def appendXbgf(tree,xbgfFile):
 if not tree:
  tree = ET.Element(slpsns.xbgf_('sequence'))
 xtree = ET.parse(xbgfFile)
 for t in xtree.findall('*'):
  tree.append(t)
 return tree
