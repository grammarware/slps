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
 for outline in tree.findall("//sample"):
  cx+=1
  if outline.attrib.has_key('id'):
   name = outline.attrib['id']
  else:
   name = 'untitled'+`cx`
  torun = open (dir+'/'+name+'.src',"w")
  for line in outline.text.split('\n'):
   if line.strip()!='':
    torun.write(line.strip()+'\n')
  torun.close()
  if outline.attrib.has_key('id'):
   library[name]=outline.text
  if outline.attrib.has_key('sort'):
   sort=outline.attrib['sort']
  else:
   sort=None
 # All executions
 for outline in tree.findall("//runnable"):
  cx+=1
  if outline.attrib.has_key('id'):
   name = outline.attrib['id']
  else:
   name = 'untitled'+`cx`
  if outline.findtext('context'):
   if not library.has_key(outline.findtext('context')):
    print "No context found for sample",name,'('+outline.findtext('context')+'), test case not used'
    continue
   else:
    con = open (dir+'/'+name+'.src','w')
    for line in library[outline.findtext('context')].split('\n'):
     if line.strip()!='':
      con.write(line.strip()+'\n')
    con.close()
  if outline.findtext('yields'):
   con = open (dir+'/'+name+'.val','w')
   con.write(outline.findtext('yields'))
   con.close()
  torun = open (dir+'/'+name+'.run','w')
  line = outline.findtext('main')
  for arg in outline.findall("argument"):
   line += ' ' + arg.text
  torun.write(line+'\n')
  torun.close()
 print cx,'samples in the test set.'

if __name__ == "__main__":
 print 'Sample Set Extractor'
 if len(sys.argv) == 3:
  unpacksamples(sys.argv[1],sys.argv[2])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<ldf>','<dir>'
  sys.exit(1)
