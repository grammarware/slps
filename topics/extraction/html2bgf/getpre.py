#!/usr/bin/python
import sys

yes = []
no = []

def checkSection(text,tagN,includeFlag,p):
 for chapter in text.split('<h'+`tagN`+'>')[1:]:
  grammar = includeFlag
  content = chapter.split('</h'+`tagN`+'>')
  for kw in yes:
   if content[0].find(kw)>=0:
    grammar = True
  for kw in no:
   if content[0].find(kw)>=0:
    grammar = False
  if grammar and content[1].find('<h')==-1:
   chunks = content[1].split('<pre>')
   for i in range(1,len(chunks)):
    if chunks[i-1].rfind('<blockquote>')!=len(chunks[i-1])-12:
     p.write(chunks[i].split('</pre>')[0].replace('<br>','').replace('&#32;',' '))
     p.write('\n<hr>')
  else:
   #print 'Going deeper than',content[0].split()[0]
   if grammar:
    chunks = content[1].split('<h'+`tagN+1`+'>')[0].split('<pre>')
    for i in range(1,len(chunks)):
     if chunks[i-1].rfind('<blockquote>')!=len(chunks[i-1])-12:
      p.write(chunks[i].split('</pre>')[0].replace('<br>','').replace('&#32;',' '))
      p.write('\n<hr>')
   checkSection(content[1],tagN+1,grammar,p)

if len(sys.argv)!=4:
 print '''This tool simulates a particular XPath query that it can execute upon a badly composed HTML.

Usage:
	python xpathpre.py <keywords-list> <input-document> <output-bgf>

It will read the input, looking for sections (<h?>) that contain keywords in the title.
Once found, it will output the content of all <pre> tags from such sections.
Keywords can be positive or negative, with positive being default.
<pre> inside <blockquote> is not used.'''
else:
 for kw in open(sys.argv[1],'r').readlines():
  kw = kw.strip()
  if not kw:
   continue
  elif kw[0]=='-':
   no.append(kw[1:])
  elif kw[0]=='+':
   yes.append(kw[1:])
  else:
   yes.append(kw)
 out = open(sys.argv[3],'w')
 out.write('<pre>')
 checkSection(''.join(open(sys.argv[2],'r').readlines()),1,False,out)
 out.write('</pre>')
 out.close()
