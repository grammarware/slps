#!/usr/bin/python
import sys
from elementtree import ElementTree

def copyfile(x,y):
 xh=open(x,'r')
 yh=open(y,'w')
 yh.writelines(xh.readlines())
 xh.close()
 yh.close()

def unpacksamples(where,dir):
 library={}
 cx = 0
 tree = ElementTree.parse(where)
 for xmlnode in tree.findall("//sample"):
  cx+=1
  if xmlnode.attrib.has_key('id'):
   name = xmlnode.attrib['id']
   library[name]=xmlnode.text
  else:
   name = 'untitled'+`cx`
  if xmlnode.attrib.has_key('sort'):
   print 'Test case',name,'of sort',xmlnode.attrib['sort'],'not extracted.'
  else:
   src = open (dir+'/'+name+'.src',"w")
   for line in xmlnode.text.split('\n'):
    if line.strip()!='':
     src.write(line.strip()+'\n')
   src.close()
   print 'Test case',name,'extracted.'
 # All executions
 for xmlnode in tree.findall("//runnable"):
  cx+=1
  if xmlnode.attrib.has_key('id'):
   name = xmlnode.attrib['id']
  else:
   name = 'untitled'+`cx`
  if xmlnode.findtext('context'):
   if not library.has_key(xmlnode.findtext('context')):
    print "No context found for sample",name,'('+xmlnode.findtext('context')+'), test case not used'
    continue
   else:
    con = open (dir+'/'+name+'.ctx','w')
    for line in library[xmlnode.findtext('context')].split('\n'):
     if line.strip()!='':
      con.write(line.strip()+'\n')
    con.close()
  if xmlnode.findtext('yields'):
   con = open (dir+'/'+name+'.val','w')
   con.write(xmlnode.findtext('yields'))
   con.close()
  run = open (dir+'/'+name+'.run','w')
  line = xmlnode.findtext('main')
  for arg in xmlnode.findall("argument"):
   line += ' ' + arg.text
  run.write(line+'\n')
  run.close()
 print cx,'samples in the test set.'

if __name__ == "__main__":
 print 'Sample Set Extractor'
 if len(sys.argv) == 3:
  unpacksamples(sys.argv[1],sys.argv[2])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<ldf>','<dir>'
  sys.exit(1)
