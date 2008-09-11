#!/usr/bin/python
import sys

yes = []
no = []

def checkSection(text,tagN,includeFlag):
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
   for chunk in content[1].split('<pre>')[1:]:
    print chunk.split('</pre>')[0].replace('<br>','').replace('&#32;',' ')
    print '<hr>'
  else:
   #print 'Going deeper than',content[0].split()[0]
   if grammar:
    for chunk in content[1].split('<h'+`tagN+1`+'>')[0].split('<pre>')[1:]:
     print chunk.split('</pre>')[0].replace('<br>','').replace('&#32;',' ')
     print '<hr>'
   checkSection(content[1],tagN+1,grammar)

if len(sys.argv)<2:
 print '''This tool simulates a particular XPath query that it can execute upon a badly composed HTML.

Usage:
	python xpathpre.py keyword [keyword ...] <input >output

It will read the input, looking for sections (<h?>) that contain keywords in the title.
Once found, it will output the content of all <pre> tags from such sections.
Keywords can be positive or negative, with positive being default.'''
else:
 for kw in sys.argv[1:]:
  if kw[0]=='-':
   no.append(kw[1:])
  elif kw[0]=='+':
   yes.append(kw[1:])
  else:
   yes.append(kw)
 print '<pre>'
 checkSection(''.join(sys.stdin.readlines()),1,False)
 print '</pre>'

