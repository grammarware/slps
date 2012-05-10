#!/usr/bin/python
import sys
import slpsns
import string
import elementtree.ElementTree as ET
from pydbgr.api import debug

"""
document:
        document-metainfo part+
document-metainfo:
        ((body number::STR) | author::STR+) topic::STR status
          (version::STR | edition::STR) previous* date::timestamp
body:
        ansi::EPSILON | ecma::EPSILON | ieee::EPSILON | iso::EPSILON | iso/iet::EPSILON | itu::EPSILON | iec::EPSILON
          | ietf::EPSILON | oasis::EPSILON | omg::EPSILON | wsa::EPSILON | w3c::EPSILON
"""
class Body:
	_val = ''
	def __init__(self,body):
		self.set(body)
		return
	def set(self,body):
		if body in ['ansi','ecma','ieee','iso','itu','iec','ietf','oasis','omg','wsa','w3c']:
			self._val = body
		return
	def get(self):
		return self._val
"""
status:
        unknown::EPSILON | draft::EPSILON | candidate::EPSILON | proposed::EPSILON | approved::EPSILON
          | revised::EPSILON | obsolete::EPSILON | withdrawn::EPSILON | collection::EPSILON
          | trial::EPSILON | errata::EPSILON | report::EPSILON
previous:
        title::STR (version::STR | edition::STR) uri::anyURI
part:
        part-metainfo section+
part-metainfo:
        part-role title::STR? author::STR* id::ID?
part-role:
        front-matter::EPSILON | core-part::EPSILON | back-matter::EPSILON | annex::EPSILON
section:
        placeholder | simple-section | lexical-section | structured-section
          | list-section
placeholder:
        index::EPSILON | full-grammar::EPSILON | list-of-tables::EPSILON | list-of-authors::EPSILON
          | list-of-contents::EPSILON | list-of-references::EPSILON
simple-section:
        section-metainfo section-content::textual-content
section-metainfo:
        section-role title::STR? author::STR* id::ID?
section-role:
        abstract::EPSILON | conformance::EPSILON | compliance::EPSILON | compatibility::EPSILON
          | document-structure::EPSILON | notation::EPSILON | normative-references::EPSILON
          | design-goals::EPSILON | scope::EPSILON | whatsnew::EPSILON | foreword::EPSILON
lexical-section:
        lexical-metainfo lexical-section-content::textual-content
lexical-metainfo:
        lexical-section-role title::STR? author::STR* id::ID?
lexical-section-role:
        line-continuations::EPSILON | whitespace::EPSILON | tokens::EPSILON
          | preprocessor::EPSILON | literals::EPSILON | lexical-issue::EPSILON
list-section:
        list-section-metainfo list-section-content::(term+)
list-section-metainfo:
        list-section-role title::STR? author::STR* id::ID?
list-section-role:
        definitions::EPSILON | abbreviations::EPSILON | language-overview::EPSILON
          | normative-references::EPSILON
term:
        name::STR definition::textual-content
structured-section:
        structured-section-metainfo structured-section-content
structured-section-metainfo:
        title::STR author::STR* id::ID?
structured-section-content:
        structured-section-element+
structured-section-element:
        subtopic::structured-section | references::list | placeholder
          | value::(key::STR data::STR) | (element-role simple-section)
element-role:
        normative-role | informative-role | specific-section::EPSILON
normative-role:
        synopsis::EPSILON | description::EPSILON | syntax::EPSILON | constraints::EPSILON
          | relationship::EPSILON | semantics::EPSILON | default::EPSILON
informative:
        rationale::EPSILON | example::EPSILON | update::EPSILON
textual-content:
        text-element+
text-element:
        empty::EPSILON | code::STR | text::mixed-type | figure | table | list
          | formula | sample::(STR source::STR) | production
mixed-type:
        (ANY | STR)*
figure:
        figure-metainfo figure-source+
figure-metainfo:
        short-caption::STR? caption::STR id::ID?
figure-type:
        PDF::EPSILON | PostScript::EPSILON | SVG::EPSILON | PNG::EPSILON | GIF::EPSILON | JPEG::EPSILON
figure-source:
        type::figure-type (local-file::STR | uri::anyURI)
table:
        header::table-row* row::table-row+
table-row:
        table-cell::textual-content+
list:
        item::mixed-type+
"""

class Bunch:
 def __init__(self, **kwds):
  self.__dict__.update(kwds)

class LDFwriter:
	"""
	document:
	        document-metainfo part+
	document-metainfo:
	        ((body number::STR) | author::STR+) topic::STR status
	          (version::STR | edition::STR) previous* date::timestamp
	"""
	_document = {'metainfo':
							{'bodynumber':
											{'body':'','number':''},
							'authors':[],
							'topic':'',
							'status':'unknown',
							'version':'',
							'edition':'',
							'previous':[],
							'date':''},
				'parts':[]}
	_dtree = None
	def write(self,fname):
		ET.ElementTree(self._dtree).write(fname)
		return
	def __init__(self):
		# requires preceding slpsns.init(ET)
		self._dtree = ET.Element('ldf:document')
		self._dtree.set('xmlns:ldf',slpsns.ldfns)
		self._dtree.set('xmlns:bgf',slpsns.bgfns)
		self._dtree.set('xmlns:html',slpsns.htmlns)
		return
	def setBodyNumber(self,body,number):
		# presence of authors means there cannot be body/number
		if self._document['metainfo']['authors'] == []:
			if body not in ['','ansi','ecma','ieee','iso','itu','iec','ietf','oasis','omg','wsa','w3c']:
				return False
			self._document['metainfo']['bodynumber']['body']   == body:
			self._document['metainfo']['bodynumber']['number'] == number:
			return True
		else:
			return False
	def dropBodyNumber(self):
		self.setBodyNumber('','')
		return True
	def getBody(self):
		return self._document['metainfo']['bodynumber']['body']
	def getNumber(self):
		return self._document['metainfo']['bodynumber']['number']
	def setAuthors(self,authors):
		# presence of body/number means there cannot be authors
		if self._document['metainfo']['bodynumber']['body']+self._document['metainfo']['bodynumber']['number'] == '':
			self._document['metainfo']['authors'] = authors[:]
			return True
		else:
			return False
	def getAuthors(self):
		return self._document['metainfo']['authors'][:]
	def dropAuthors(self):
		self.setAuthors([])
		return True
	def setTopic(self,topic):
		self._document['metainfo']['topic'] = topic
		return True
	def getTopic(self):
		return self._document['metainfo']['topic']
	def setStatus(self,status):
		if status in ['unknown','draft','candidate','proposed','approved','revised','obsolete','withdrawn','collection','trial','errata','report']:
			self._document['metainfo']['status'] = status
			return True
		else:
			return False
	def getStatus(self):
		return self._document['metainfo']['status']
	def setVersion(self,version):
		if self._document['metainfo']['edition']:
			return False
		self._document['metainfo']['version'] = version
		return True
	def setEdition(self,edition):
		if self._document['metainfo']['version']:
			return False
		self._document['metainfo']['edition'] = edition
		return True
	def getVersion(self):
		return self._document['metainfo']['version']
	def getEdition(self):
		return self._document['metainfo']['edition']

def addStructuredSection(name,title,sid,parent,hlines):
	"""
	structuredSection:
	        title::STR author::STR* structuredSectionElement+ id::STR?
	structuredSectionElement:
	        placeholder::generated
	        normative
	        informative
	        production
	        references::simpleList
	        section::simpleSection
	        subtopic::structuredSection
	        value::(key::STR data::STR)
	normative:
	        synopsis::simpleSection
	        description::simpleSection
	        syntax::simpleSection
	        constraints::simpleSection
	        relationship::simpleSection
	        semantics::simpleSection
	        default::simpleSection
	informative:
	        rationale::simpleSection
	        example::simpleSection
	        update::simpleSection
	"""
	ts = ET.SubElement(parent,name)
	if title:
		ET.SubElement(ts,'title').text = title
	#TODO insides
	text = ET.SubElement(ET.SubElement(ts,'description'),'content')
	if hlines[0].find('</h2>'):
		chunk = ' '.join(hlines[1:])
	else:
		chunk = ' '.join(hlines)
	convertContent(cleanupText(chunk),text)
	#ET.SubElement(text,'text').text = 'text placeholder to avoid junk'
	if sid:
		ts.set('id',sid)
	return ts

def convertContent(html,xml):
	chunk = html[:]
	last = None
	# TODO general algorithm to map HTML contents into LDF contents
	while chunk != '':
		tags, sliced, chunk = chop(chunk)
		if tags == []:
			# TODO ?
			if last != None:
				last.tail = cleanupText(sliced)
			else:
				xml.text = cleanupText(sliced)
		elif tags[0] == 'p':
			last = ET.SubElement(xml,'text')
			convertContent(sliced,last)
		elif tags[0] == 'code':
			last = ET.SubElement(xml,'code')
			convertContent(sliced,last)
		elif tags[0] == 'ul':
			last = ET.SubElement(xml,'list')
			convertContent(sliced,last)
		elif tags[0] == 'li':
			last = ET.SubElement(xml,'item')
			#debug()
			convertContent(sliced,last)
		elif tags[0] == 'a':
			ats = parseAttributes(tags[1:])
			if 'href' not in ats.keys():
				xml.set('id',ats['name'])
				continue
			last = ET.SubElement(xml,'link')
			ET.SubElement(last,'text').text = cleanupText(sliced)
			if ats['href'][0]=='#':
				ET.SubElement(last,'reference').text = ats['href'][1:]
			else:
				ET.SubElement(last,'external').text = ats['href']
			#print '>>>>>'+sliced+'<<<<<'
			#debug()
		else:
			print 'Dont know about tag',tags[0]
			pass
	#debug()
	return

def cleanupText(s):
	return reduce(lambda x,y:x.replace(y,''),['<i>','</i>','<b>','</b>'],s)

def parseAttributes(a):
	d = {}
	for x in a:
		name,value = x.split('="')
		d[name] = value[:-1]
	return d

def chop(raw):
	if raw.find('</body>')>-1:
		return [],raw[:raw.index('</body>')].strip(),''
	#raw = raw.strip()
	if raw[0]=='<':
		tags = raw[1:raw.find('>')].split()
		if tags[0] in ('img','br'):
			sliced = ''
			rest = raw[len(' '.join(tags)):]
		opening = '<'+tags[0]+'>'
		closing = '</'+tags[0]+'>'
		sliced = raw[raw.index('>')+1:raw.index(closing)]
		rest = raw[raw.index(closing)+len(closing):]
		# correction for <x>a<x>b</x>c</x><x>d</x>
		while sliced.count(opening) != sliced.count(closing):
			sliced += closing+rest.split(closing)[0]
			rest = rest[rest.index(closing)+len(closing):]
	elif raw.find('<') > 0:
		tags = []
		sliced = raw[:raw.index('<')]
		rest = raw[raw.index('<'):]
	else:
		tags = []
		sliced = raw
		rest = ''
	return tags,sliced,rest

def addTopSection(name,title,sid,parent,lines):
	"""
	simpleSection:
	        title::STR? author::STR* content::simpleText id::STR?
	"""
	if parent.findall('part/core') or parent.findall('core'):
		# back matter
		matter = parent.findall('backMatter')
		if matter:
			point = matter[0]
		else:
			point = ET.SubElement(parent,'backMatter')
	else:
		# front matter
		matter = parent.findall('frontMatter')
		if matter:
			point = matter[0]
		else:
			point = ET.SubElement(parent,'frontMatter')
	ts = ET.SubElement(point,name)
	if name == 'placeholder':
		# the only top section that is not a simple section
		if title == 'Table of contents':
			ts.text = 'listofcontents'
			return ts
		else:
			# TODO
			print 'Unknown placeholder!'
			return ts
	if title:
		ET.SubElement(ts,'title').text = title
	# TODO content
	text = ET.SubElement(ts,'content')
	for line in lines:
		#???
		pass
	ET.SubElement(text,'text').text = 'text placeholder to avoid junk'
	if sid:
		ts.set('id',sid)
	return ts

def mapSection(section,dtree,lines):
	# idea: make a class that would accept methods like addLexicalSection() and add lexical part if that's the first lexical section
	number,title,anchor,where = section
	# pseudocode beware!
	# TODO: separate top sections that go to front matter from those that go to back matter
	if title == 'Abstract':
		addTopSection('abstract',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif title == 'Status of this document':
		addTopSection('scope',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif title == 'Table of contents':
		addTopSection('placeholder',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif title == 'Introduction':
		addTopSection('foreword',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif title == 'References':
		addTopSection('normativeReferences',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif title == 'Conformance':
		addTopSection('conformance',title,anchor,dtree,before('PROCESSED',lines[where:]))
	elif number.isupper():
		addStructuredSection('annex',title,anchor,dtree,before('PROCESSED',lines[where:]))
	else:
		addStructuredSection('core',title,anchor,dtree,before('PROCESSED',lines[where:]))
		#print 'Unrecognised type of section:',title
	return
	
def makeFrontMatter(dtree,lines):
	"""
	topSection:
	        foreword::simpleSection
	        designGoals::simpleSection
	        scope::simpleSection
	        conformance::simpleSection
	        compliance::simpleSection
	        compatibility::simpleSection
	        notation::simpleSection
	        normativeReferences::simpleSection
	        documentStructure::simpleSection
	        whatsnew::simpleSection
	        placeholder::generated
	"""
	return
	
def scanSections(lines):
	sections = []
	for i in range(0,lines.count('<h2>')):
		line = lines[lines.index('<h2>')+1]
		anchor,title = unattag('a','name',line)
		if title == '':
			if line.find('<a name="'+anchor+'"></a>')>-1:
				title = line[line.find('<a name="'+anchor+'"></a>')+len('<a name="'+anchor+'"></a>'):line.find('</h2>')]
			else:
				title = 'Could not guess :-('
		# remove possible leading section number
		try:
			number = str(int(title.split()[0]))
			title = title[title.index(' ')+1:]
		except ValueError:
			number = ''
		if len(title.split()[0])==1 and title.split()[0].isupper():
			# 'A', 'B', etc
			number = title.split()[0]
			title = title[title.index(' ')+1:]			
		#print 'Section "'+title+'" at #'+anchor+', line',lines.index('<h2>')
		sections.append((number,title,anchor,lines.index('<h2>')+1))
		lines[lines.index('<h2>')] = 'PROCESSED'
	return sections

def makeTitlePage(dtree,lines):
	"""
	titlePage:
		((body::body number::STR) | author::STR+) topic::STR (version::STR | edition::STR) status::status date::STR
	"""
	content = {}
	subtitle = untag('h2',grep('</h2>',grep('<h2>',lines))[0]).split()
	content['body'] = subtitle[0].lower()
	content['date'] = ' '.join(subtitle[-3:])
	status = ' '.join(subtitle[1:-3])
	if status == 'Recommendation':
		content['status'] = 'approved'
	elif status == 'Working Draft':
		content['status'] = 'proposed'
	#...
	else:
		content['status'] = 'unknown'
	content['topic'] = untag('h1',grep('h1',lines)[0])
	if content['topic'].find('<br>')>-1:
		if content['topic'].find('<br>Version')>-1:
			content['topic'],content['version'] = content['topic'].split('<br>Version ')
		#elif ... edition?
	if content['topic'].find('(')>-1:
		content['topic'],content['number'] = content['topic'].split(' (')
		content['number'] = content['number'][:-1]
	else:
		content['number'] = '?'
	return dictionaryToXml(dtree,'titlePage',content,['body','number','topic','version','status','date'])

def dictionaryToXml(parent,name,content,order):
	e = ET.SubElement(parent,name)
	for k in order:
		ET.SubElement(e,k).text = content[k]
	return e

def startLDF():
	dtree = ET.Element('ldf:document')
	dtree.set('xmlns:ldf',slpsns.ldfns)
	dtree.set('xmlns:bgf',slpsns.bgfns)
	dtree.set('xmlns:html',slpsns.htmlns)
	return dtree

def dumpLDF(dtree,ldffile):
	ET.ElementTree(dtree).write(ldffile)
	return

def readLines(htmlfile):
	return filter(lambda x:x!='',map(string.strip,open(sys.argv[1],'r').readlines()))
	
def grep(w,a):
	return filter(lambda x:x.find(w)>-1,a)

def before(w,a):
	l = []
	for x in a:
		if x.find(w)>-1:
			break
		else:
			l.append(x)
	return l

def untag(tag,s):
	return s[s.find('<'+tag+'>')+len(tag)+2:s.find('</'+tag+'>')]

def unattag(tag,at,s):
	return s[s.find('<'+tag+' '+at+'="')+len('<'+tag+' '+at+'="'):s.find('">')],s[s.find('">')+2:s.find('</'+tag+'>')]

if __name__ == "__main__":
	if len(sys.argv) == 3:
		slpsns.init(ET)
		#apply(main,sys.argv[1:4])
		lines = readLines(sys.argv[1])
		ldf = startLDF()
		makeTitlePage(ldf,lines)
		for section in scanSections(lines):
			sec = mapSection(section,ldf,lines)
		makeFrontMatter(ldf,lines)
		dumpLDF(ldf,sys.argv[2])
	else:
		print 'This tool maps W3C-like hypertext documents to Language Definition Format documents.\n\nUsage:'
		print ' ',sys.argv[0],'<input xsd file>','<input bgf file>','<output ldf file>'
		sys.exit(1)
