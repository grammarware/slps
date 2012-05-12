#!/Library/Frameworks/Python.framework/Versions/3.1/bin/python3
import os
import sys
sys.path.append(os.getcwd().split('slps')[0]+'slps/shared/python')
import slpsns
import xml.etree.ElementTree as ET
import BGF3,XBGF3,MBGF

namemap = {}

def applynamemap(e):
	if e.who()=='Expression':
		applynamemap(e.wrapped)
		return
	elif e.who()=='Nonterminal':
		if e.data in namemap:
			e.data = namemap[e.data]
		return
	print('[MBGF] applynamemap error:',e.who())
	sys.exit(1)

def wrapexp(exp,mod):
	if mod=='1':
		return str(exp)
	else:
		return str(exp)+mod
		# return '('+str(exp)+')'+mod

if __name__ == "__main__":
	if len(sys.argv)!=5:
		print('This tool needs MBGF as an input, XBGF as an output and a number of expected BGF input')
		print('\nUsage:')
		print('	',sys.argv[0],'<input-mbgf>','<in-num>','<out-num>','<output-xbgf>')
		sys.exit(1)
	else:
		mbgf = ET.parse(sys.argv[1])
		inname = sys.argv[2]
		outname = sys.argv[3]
		xbgf = XBGF3.Sequence()
		sources = {}
		predicates = []
		print('Reading the predicates...')
		for e in mbgf.findall('*'):
			if e.tag=='sources':
				predicates.append(MBGF.Sources(e))
				# for s in e.findall('src'):
				# 	sources[s.attrib['name']] = s.text
			elif e.tag=='naming-convention':
				predicates.append(MBGF.NamingConvention(e))
			elif e.tag=='width':
				predicates.append(MBGF.Width(e))
			elif e.tag=='unification':
				predicates.append(MBGF.Unification(e))
			elif e.tag=='iteration':
				predicates.append(MBGF.Iteration(e))
			else:
				print('Predicate',e.tag,'not supported.')
				# print(sources)
				sys.exit(1)
		# executing
		print('Inferring unidirectional grammar transformations from',inname,'to',outname,'...')
		pbyid = {}
		unscheduled = []
		for p in predicates:
			# print(p.who(),'#',p.id,'@',p.depends)
			if p.who() == 'Sources':
				sources = p
			else:
				unscheduled.append(p.id)
				pbyid[p.id] = p
		scheduled = []
		while len(unscheduled)>0:
			for i in range(0,len(unscheduled)):
				candidate = pbyid[unscheduled[i]]
				if not(candidate.depends) or candidate.depends in scheduled:
					scheduled.append(unscheduled[i])
					unscheduled.remove(unscheduled[i])
					break
			else:
				print('Failed to schedule',unscheduled)
				sys.exit(2)
		print('Scheduled order:',scheduled)
		for id in scheduled:
			pass

		ET.ElementTree(xbgf.getXml()).write(sys.argv[4])
		print('Success',predicates)
        