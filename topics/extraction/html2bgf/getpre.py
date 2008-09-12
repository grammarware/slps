#!/usr/bin/python
import sys

yes = []
no = []
counter={}

def processSection(text,tagN,cx,p):
 # text - section text
 # tagN - <h1>, <h2>, etc
 # cx - parent counter
 # p - file output
 max = cx
 content = text.split('</h'+`tagN`+'>')
 for kw in yes:
  if content[0].split()[0]==kw:
   max = counter[kw]
   break
 for kw in no:
  if content[0].find(kw)==0:
   max = 0
 subsections=content[1].split('<h'+`tagN+1`+'>')
 for pre in subsections[0].split('<pre>')[1:]:
  if max:
   p.write(pre.split('</pre>')[0].replace('<br>','').replace('&#32;',' '))
   p.write('\n<hr>')
   max -= 1
  else:
   #print 'Skipped <pre> in',content[0].split()[0]
   pass
 for ss in subsections[1:]:
  processSection(ss,tagN+1,max,p)

if len(sys.argv)!=4:
 print '''This tool simulates a particular XPath query that it can execute upon a badly composed HTML.

Usage:
	python xpathpre.py <keywords-list> <input-document> <output-bgf>

It will read the input, looking for sections (<h?>) that contain keywords in the title.
Once found, it will output the content of <pre> tags from such sections.
Keywords can be negative: -keyword will make it skip sections with a keyword.
Keyword/N means that the first N <pre> tags will be copied from a matching section.
Keyword/* means all <pre>, Keyword without a slash means only the first <pre>.'''
else:
 for kw in open(sys.argv[1],'r').readlines():
  kw = kw.strip()
  if not kw:
   continue
  elif kw[0]=='-':
   no.append(kw[1:])
  else:
   if kw in yes:
    counter[kw]+=1
   elif kw.find('/')>0:
    pair = kw.split('/')
    yes.append(pair[0])
    if pair[1]=='*':
     counter[pair[0]]=1000
    else:
     counter[pair[0]]=int(pair[1])
   else:
    yes.append(kw)
    counter[kw]=1
 counter['start']=1
 out = open(sys.argv[3],'w')
 out.write('<pre>')
 #checkSection(''.join(open(sys.argv[2],'r').readlines()),1,False,out,'start')
 for toplevel in ''.join(open(sys.argv[2],'r').readlines()).split('<h1>')[1:]:
  processSection(toplevel,1,0,out)
 out.write('</pre>')
 out.close()
