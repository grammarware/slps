#!/usr/local/bin/python
# -*- coding: utf-8 -*-

def removetags(s):
	# return s
	s = s.replace('&#8220;','“').replace('&#8221;','”')
	# <span xmlns="" class="nt">
	# s = s.replace('<span xmlns="" class="nt">','[[')
	# s = s.replace('</span>',']]')
	while s.find('<')>-1:
		c = s.split('<')
		# ```<a name="abridge" id="abridge">[[abridge]]</a>:
		#    '
		c[0] = c[0]+c[1].split('>')[1]
		if len(c)>2:
			s = '<'.join([c[0]]+c[2:])
		else:
			s = c[0]
	return s

f = open('index.html','r')
txt = {}
now = ''
raw = False
for r in f.readlines():
	if not raw:
		r = r.strip()
	else:
		r = r.replace('\n','')
	if r.startswith('<pre>'):
		raw = True
	if r.find('</pre>')>-1:
		raw = False
	if r.startswith('<a name="') and r.endswith('</h3>'):
		now = r.split('"')[1]
		print 'Found',now+'!'
		txt[now] = []
		continue
	if r.find('compatibility-section')>-1:
		now = ''
		continue
	# refactor
	if r.endswith('</h4>'):
		r = '### '+r.split('<')[-2].split('>')[-1]
	r = r.replace('<pre>','```')
	r = r.replace('</pre>','```')
	r = removetags(r)
	# use
	if now!='':
		txt[now].append(r)
	# print 'Proceessing',r[:20]+'... as',now
f.close()

# print txt

for k in txt.keys():
	f = open('texts/'+k.capitalize()+'.md','w')
	for r in txt[k]:
		f.write(r.replace('&lt;','<').replace('&gt;','>')+'\n')
	f.close()

