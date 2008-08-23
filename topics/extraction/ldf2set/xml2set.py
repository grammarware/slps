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
  torun = open (dir+'/sample'+`cx`+'.src',"w")
  for line in outline.text.split('\n'):
   if line.strip()!='':
    torun.write(line.strip()+'\n')
  torun.close()
  if outline.attrib.has_key('id'):
   library[outline.attrib['id']]=outline.text
  if outline.attrib.has_key('sort'):
   sort=outline.attrib['sort']
  else:
   sort=None
 # All executions
 for outline in tree.findall("//runnable"):
  if outline.findtext('context'):
   if not library.has_key(outline.findtext('context')):
    print "No context found for sample",cx,'('+outline.findtext('context')+'), test case not used'
    continue
   else:
    con = open (dir+'/sample'+`cx`+'.src','w')
    for line in library[outline.findtext('context')].split('\n'):
     if line.strip()!='':
      con.write(line.strip()+'\n')
    con.close()
  if outline.findtext('yields'):
   con = open (dir+'/sample'+`cx`+'.val','w')
   con.write(outline.findtext('yields'))
   con.close()
  torun = open (dir+'/sample'+`cx`+'.run','w')
  line = outline.findtext('main')
  for arg in outline.findall("argument"):
   line += ' ' + arg.text
  torun.write(line+'\n')
  torun.close()
  cx+=1
 print cx,'samples in the test set.'

if __name__ == "__main__":
 print 'Sample Set Extractor'
 if len(sys.argv) == 3:
  unpacksamples(sys.argv[1],sys.argv[2])
 else:
  print 'Usage:'
  print ' ',sys.argv[0],'<ldf>','<dir>'
  sys.exit(1)
