#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os, sys, math
import slpsns
import elementtree.ElementTree as ET
import BGF
import metrics

class Bunch:
	def __init__(self, **kwds):
		self.__dict__.update(kwds)
	def extend(self, **kwds):
		self.__dict__.update(kwds)

class EmptyBench:
	cache = None
	vals = None
	def __init__(self,g):
		# the grammar
		self.grammar = g
		# other language stuff like call graphs and various sets
		if not self.cache:
			self.cache = Bunch()
		# preliminary sizing metrics
		if not self.vals:
			self.vals = Bunch()
		# dynamic metrics?
	
# Simple measurements: TERM, VAR, LAB, ROOT, UNDEF, DEAD, DEADP, …
class MeasurementBench (EmptyBench):
	def __init__(self,g):
		EmptyBench.__init__(self,g)
		# other language stuff like call graphs and various sets
		self.cache.extend(tops=None,bottoms=None,deadtops=None,defd=None,nrus=None,varlist=None)
		# preliminary sizing metrics
		self.vals.extend(term=None,var=None,lab=None,prod=None,dead=None,deadp=None,undef=None,root=None,avsn=None,avsp=None,loc=None)
		# dynamic metrics?
	def _getTops(self):
		if not self.cache.tops:
			self.cache.tops = metrics.setminus(self._getDefds(),self._getNRUs())
		return self.cache.tops
	def _getDeadTops(self):
		if not self.cache.deadtops:
			self.cache.deadtops = metrics.setminus(self._getTops(),self.grammar.roots)
		return self.cache.deadtops
	def _getDefds(self):
		if not self.cache.defd:
			self.cache.defd = metrics.defd(self.grammar)
		return self.cache.defd
	def _getNRUs(self):
		if not self.cache.nrus:
			self.cache.nrus = metrics.nrused(self.grammar)
		return self.cache.nrus
	def _getBottoms(self):
		if not self.cache.bottoms:
			self.cache.bottoms = metrics.setminus(self._getNRUs(),self._getDefds())
		return self.cache.bottoms
	def TERM(self):
		if not self.vals.term:
			self.vals.term = metrics.TERM(self.grammar)
		return self.vals.term
	def _getVarList(self):
		if not self.cache.varlist:
			self.cache.varlist = metrics.var(self.grammar)
		return self.cache.varlist
	def VAR(self):
		if not self.vals.var:
			self.vals.var = len(self._getVarList()) + metrics.VAL(self.grammar)
		return self.vals.var
	def LAB(self):
		if not self.vals.lab:
			self.vals.lab = metrics.LAB(self.grammar)
		return self.vals.lab
	def PROD(self):
		if not self.vals.prod:
			self.vals.prod = metrics.PROD(self.grammar)
		return self.vals.prod
	def UNDEF(self):
		if not self.vals.undef:
			self.vals.undef = len(self._getBottoms())
		return self.vals.undef
	def DEAD(self):
		if not self.vals.dead:
			self.vals.dead = len(self._getDeadTops())
		return self.vals.dead
	def DEADP(self):
		if not self.vals.deadp:
			self.vals.deadp = 0
			for p in self.grammar.prods:
				if p.nt in self._getDeadTops():
					self.vals.deadp += 1
		return self.vals.deadp
	def ROOT(self):
		if not self.vals.root:
			self.vals.root = len(self.grammar.roots)
		return self.vals.root
	def AVSN(self):
		if not self.vals.avsn:
			self.vals.avsn = sum(map(metrics.rhssize,self.grammar.prods))/(0.0+self.VAR())
		return self.vals.avsn
	def AVSP(self):
		if not self.vals.avsp:
			self.vals.avsp = sum(map(metrics.rhssize,self.grammar.prods))/(0.0+self.PROD())
		return self.vals.avsp
	def LOC(self):
		if not self.vals.loc:
			self.vals.loc = self.VAR() + self.PROD()
		return self.vals.loc


class HalsteadBench (MeasurementBench):
	def __init__(self,g):
		MeasurementBench.__init__(self,g)
		# other language stuff like call graphs and various sets
		self.cache.extend(umeta=None)
		# preliminary sizing metrics
		self.vals.extend(umet=None,uops=None,met=None,ops=None,len=None,voc=None,lenhat=None,pur=None,uopsmin=None,vol=None,pvol=None,bvol=None,hlev=None,hlevhat=None,dif=None,llev=None,ic=None,eff=None,effhat=None,bug1=None,bug2=None)
	#
	def getUMetaList(self):
		if not self.cache.umeta:
			self.cache.umeta = metrics.allOperators(self.grammar)
			self.cache.umeta.sort()
		return self.cache.umeta
	def UMET(self):
		if not self.vals.umet:
			self.vals.umet = len(self.getUMetaList())
		return self.vals.umet
	def UOPS(self):
		if not self.vals.uops:
			self.vals.uops = self.VAR() + self.TERM() + self.LAB()
		return self.vals.uops
	def MET(self):
		if not self.vals.met:
			self.vals.met = metrics.opr(self.grammar)
		return self.vals.met
	def OPS(self):
		if not self.vals.ops:
			self.vals.ops = metrics.opd(self.grammar)
		return self.vals.ops
	def PUR(self):
		if not self.vals.pur:
			self.vals.pur = self.LENHAT()/(0.0+self.LEN())
		return self.vals.pur
	def LEN(self):
		if not self.vals.len:
			self.vals.len = self.MET() + self.OPS()
		return self.vals.len
	def VOC(self):
		if not self.vals.voc:
			self.vals.voc = self.UMET() + self.UOPS()
		return self.vals.voc
	def LENHAT(self):
		if not self.vals.lenhat:
			if self.UMET() < 1:
				print '[????] log UMET problem in LENHAT'
				return -1
			if self.UOPS() < 1:
				print '[????] log UOPS problem in LENHAT'
				return -1
			self.vals.lenhat = self.UMET()*math.log(self.UMET(),2) + self.UOPS()*math.log(self.UOPS(),2)
		return self.vals.lenhat
	def UOPSMIN(self):
		if not self.vals.uopsmin:
			self.vals.uopsmin = self.TERM() + self.ROOT() + self.UNDEF()
			if self.vals.uopsmin == 0:
				# Using the dead instead of the estimation because there are no roots
				self.vals.uopsmin = self.DEAD()
		return self.vals.uopsmin
	def VOL(self):
		if not self.vals.vol:
			if self.VOC() < 1:
				print '[????] log VOC problem in VOL'
				return -1
			self.vals.vol = self.LEN()*math.log(self.VOC(),2)
		return self.vals.vol
	def PVOL(self):
		if not self.vals.pvol:
			self.vals.pvol = (2+self.UOPSMIN())*math.log(2+self.UOPSMIN(),2)
		return self.vals.pvol
	def BVOL(self):
		if not self.vals.bvol:
			if self.UOPSMIN() < 1:
				print '[????] log UOPSMIN problem in BVOL'
				return -1
			self.vals.bvol = (2+self.UOPSMIN()*math.log(self.UOPSMIN(),2))*math.log(2+self.UOPSMIN(),2)
		return self.vals.bvol
	def HLEV(self):
		if not self.vals.hlev:
			self.vals.hlev = self.PVOL()/(0.0+self.VOL())
		return self.vals.hlev
	def HLEVHAT(self):
		if not self.vals.hlevhat:
			self.vals.hlevhat = (2.0+self.UOPS())/(self.UMET()+self.OPS())
		return self.vals.hlevhat
	def DIF(self):
		if not self.vals.dif:
			self.vals.dif = 1.0/self.HLEV()
		return self.vals.dif
	def LLEV(self):
		if not self.vals.llev:
			self.vals.llev = 1000*self.HLEV()/(1.0+self.PVOL())
		return self.vals.llev
	def IC(self):
		if not self.vals.ic:
			self.vals.ic = self.HLEVHAT() * self.VOL()
		return self.vals.ic
	def EFF(self):
		if not self.vals.eff:
			self.vals.eff = self.VOL() / (1.0+self.HLEV())
		return self.vals.eff
	def EFFHAT(self):
		if not self.vals.effhat:
			self.vals.effhat = self.VOL() / (1.0+self.HLEVHAT())
		return self.vals.effhat
	def BUG1(self):
		if not self.vals.bug1:
			self.vals.bug1 = (self.EFF()**(2.0/3))/3000.
		return self.vals.bug1
	def BUG2(self):
		if not self.vals.bug2:
			self.vals.bug2 = self.VOL()/3000.
		return self.vals.bug2
	
#	def _getSSS(self):
#		if not self.cache.sss:
#			self.cache.sss = metrics.shortestSamples(self.grammar,self._getVarList())
#		return self.cache.sss
#	def getSS(self):
#		if not self.size.ss:
#			if metrics.numbers(self._getSSS().values()):
#				self.size.ss = sum(self._getSSS().values())/self.getProd()
#			else:
#				return '?'
#		return self.size.ss
#	def getSSM(self):
#		if not self.size.ss:
#			if metrics.numbers(self._getSSS().values()):
#				self.size.ss = max(self._getSSS().values())
#			else:
#				return '?'
#		return self.size.ss
#

# A class for benchmarking fully recovered grammars	
class PatternBench(MeasurementBench):
	def __init__(self,g):
		MeasurementBench.__init__(self,g)
		self.cache.extend(patterns=None)
		self.vals.extend(npat=None,npatc=None,mpat=None,mpatc=None,wpat=None)
	#
	def listOfPats(self):
		return self._getPatterns()
	def _getPatterns(self):
		if not self.cache.patterns:
			self.cache.patterns = {}
			for p in self.grammar.prods:
				if p.expr.wrapped.__class__.__name__ == 'Choice':
					exprs = []
					for e in p.expr.wrapped.data:
						if e.wrapped.__class__.__name__ == 'Sequence':
							exprs.append(e.wrapped.data)
						else:
							exprs.append(e.wrapped)
				elif p.expr.wrapped.__class__.__name__ == 'Sequence':
					exprs = [p.expr.wrapped.data]
				else:
					exprs = [p.expr.wrapped]
				for e in exprs:
					pat = metrics.buildpattern(e)
					if pat not in self.cache.patterns.keys():
						self.cache.patterns[pat] = 0
					self.cache.patterns[pat] += 1
		return self.cache.patterns
	def NPAT(self):
		# number of patterns
		if not self.vals.npat:
			self.vals.npat = len(self._getPatterns().keys())
		return self.vals.npat
	def NPATC(self):
		# number of patterns normalised by number of nonterminals / productions?
		return 100.0*self.NPAT()/self.PROD()
	def WPAT(self):
		# length of the longest pattern
		if not self.vals.wpat:
			self.vals.wpat = max(map(len,self._getPatterns().keys()))
		return self.vals.wpat
	def MPAT(self):
		# maximum number of uses per pattern
		if not self.vals.mpat:
			self.vals.mpat = max(self._getPatterns().values())
		return self.vals.mpat
	def MPATC(self):
		# max number of uses normalised by number of nonterminals / productions?
		return 100.0*self.MPAT()/self.PROD()
	
class CFGBench(MeasurementBench):
	def __init__(self,g):
		MeasurementBench.__init__(self,g)
		self.cache.extend(fanin=None,fanout=None,lfanin=None,lfanout=None)
		self.vals.extend(fimin=None,fiavg=None,fimax=None,fomin=None,foavg=None,fomax=None,leaf=None,once=None)
	#
	def gfi(self):
		return self._getFanIn()
	def gfo(self):
		return self._getFanOut()
	def _getFanOut(self):
		if not self.cache.fanout:
			self.cache.fanout = {}
			for v in self._getVarList():
				self.cache.fanout[v] = []
			for p in self.grammar.prods:
				for nt in p.expr.getXml().findall('.//nonterminal'):
					if nt.text not in self.cache.fanout[p.nt]:
						self.cache.fanout[p.nt].append(nt.text)
			for r in self.grammar.roots:
				self.cache.fanout[r].append(r)
		return self.cache.fanout
	def _getFanIn(self):
		if not self.cache.fanin:
			self.cache.fanin = {}
			for v in self._getVarList():
				self.cache.fanin[v] = []
			self._getFanOut()
			for v in self.cache.fanout.keys():
				for n in self.cache.fanout[v]:
					self.cache.fanin[n].append(v)
		return self.cache.fanin
	def _getFanInN(self):
		if not self.cache.lfanin:
			self.cache.lfanin = map(len,self._getFanIn().values())
		return self.cache.lfanin
	def _getFanOutN(self):
		if not self.cache.lfanout:
			self.cache.lfanout = map(len,self._getFanOut().values())
		return self.cache.lfanout
	def FIMIN(self):
		if not self.vals.fimin:
			l = filter(lambda x:x>=2,self._getFanInN())
			if len(l)==0:
				self.vals.fimin = 1
			else:
				self.vals.fimin = min(l)
		return self.vals.fimin
	def FIAVG(self):
		if not self.vals.fiavg:
			self.vals.fiavg = sum(self._getFanInN())/(0.0+len(self._getVarList()))
		return self.vals.fiavg
	def FIMAX(self):
		if not self.vals.fimax:
			self.vals.fimax = max(self._getFanInN())
		return self.vals.fimax
	def ONCE(self):
		if not self.vals.once:
			self.vals.once = len(filter(lambda x:x==1,self._getFanInN()))
		return self.vals.once
	def FOMIN(self):
		if not self.vals.fomin:
			l = filter(lambda x:x!=0,self._getFanOutN())
			if len(l)==0:
				self.vals.fomin = 0
			else:
				self.vals.fomin = min(l)
		return self.vals.fomin
	def FOAVG(self):
		if not self.vals.foavg:
			self.vals.foavg = sum(self._getFanOutN())/(0.0+len(self._getVarList()))
		return self.vals.foavg
	def FOMAX(self):
		if not self.vals.fomax:
			self.vals.fomax = max(self._getFanOutN())
		return self.vals.fomax
	def LEAF(self):
		if not self.vals.leaf:
			self.vals.leaf = len(filter(lambda x:x==0,self._getFanOutN()))
		return self.vals.leaf

#
class CallGraphBench(MeasurementBench):
	def __init__(self,g):
		MeasurementBench.__init__(self,g)
		self.cache.extend(cg=None,ccg=None,levels=None,adg=None)
		self.vals.extend(lev=None,clev=None,rlev=None,nlev=None,hei=None,dep=None,timpi=None,timp=None,rec=None,recn=None,mrec=None)
	#
	def _getCallGraph(self):
		if not self.cache.cg:
			self.cache.cg = metrics.getCallGraph(self.grammar)
		return self.cache.cg
	def _getClosedCallGraph(self):
		if not self.cache.ccg:
			self.cache.ccg = metrics.getClosure(self._getCallGraph())
		return self.cache.ccg
	def _getLevels(self):
		if not self.cache.levels:
			self.cache.levels = metrics.calculateLevels(self._getClosedCallGraph())
		return self.cache.levels
	def LEV(self):
		if not self.vals.lev:
			self.vals.lev = len(self._getLevels())
		return self.vals.lev
	def	CLEV(self):
		if not self.vals.clev:
			self.vals.clev = 100.0*self.LEV()/len(self._getVarList())
		return self.vals.clev
	def DEP(self):
		if not self.vals.dep:
			self.vals.dep = max(map(len,self._getLevels()))
		return self.vals.dep
	def _getADG(self):
		if not self.cache.adg:
			self.cache.adg = []
			for i in range(0,len(self._getLevels())):
				self.cache.adg.append([])
				for j in range(0,len(self.cache.levels)):
					if i == j:
						# to ensure acyclicity
						continue
					for n in self.cache.levels[i]:
						for m in self.cache.levels[j]:
							if m in self.cache.cg[n] and j not in self.cache.adg[i]:
								self.cache.adg[i].append(j)
		#print self.cache.adg
		return self.cache.adg
	def HEI(self):
		if not self.vals.hei:
			paths = []
			for i in range(0,len(self._getADG())):
				paths.append(metrics.longest(i,self.cache.adg))
			self.vals.hei = max(paths)
		return self.vals.hei
	def	RLEV(self):
		if not self.vals.rlev:
			self.vals.rlev = len(filter(lambda x:(len(x)>1)or(x[0] in self._getCallGraph()[x[0]]),self._getLevels()))
		return self.vals.rlev
	def NLEV(self):
		if not self.vals.nlev:
			self.vals.nlev = len(filter(lambda x:len(x)>1,self._getLevels()))
		return self.vals.nlev
	def TIMPI(self):
		if not self.vals.timpi:
			self.vals.timpi = metrics.impurityOfCallGraph(self._getCallGraph())
		return self.vals.timpi
	def TIMP(self):
		if not self.vals.timp:
			self.vals.timp = metrics.impurityOfCallGraph(self._getClosedCallGraph())
		return self.vals.timp
	def REC(self):
		# sum on the number of self-references for each nonterminal
		if not self.vals.rec:
			self.vals.rec = 0
			for nt in self._getVarList():
				for p in self.grammar.prods:
					if p.nt == nt:
						for ntin in p.expr.wrapped.getXml().findall('.//nonterminal'):
							if ntin.text == nt:
								self.vals.rec += 1
		return self.vals.rec
	def RECN(self):
		# number of recursive nonterminals
		if not self.vals.recn:
			self.vals.recn = 0
			for level in self._getLevels():
				if len(level)>1:
					self.vals.recn += len(level)
				elif level[0] in self._getCallGraph()[level[0]]:
					self.vals.recn += 1
		return self.vals.recn
	def MREC(self):
		# number of mutually recursive nonterminals
		if not self.vals.mrec:
			self.vals.mrec = 0
			for level in self._getLevels():
				self.vals.mrec += len(level)*len(level)
		return self.vals.mrec


class MiscBench(MeasurementBench):
	def __init__(self,g):
		MeasurementBench.__init__(self,g)
		#self.cache.extend(cg=None,ccg=None,levels=None)
		self.vals.extend(mcc=None,mi=None)
	#
	def MCC(self):
		if not self.vals.mcc:
			self.vals.mcc = metrics.MCC(self.grammar)/(0.0+self.VAR())
		return self.vals.mcc
	def MI(self):
		# Maintainability Index = 171 - 5.2 * ln(Halstead Volume) - 0.23 * (Cyclomatic valslexity) - 16.2 * ln(Lines of Code)
		# Maintainability Index = MAX(0,(171 - 5.2 * ln(Halstead Volume) - 0.23 * (Cyclomatic valslexity) - 16.2 * ln(Lines of Code))*100 / 171)
		if self.VOL() < 1:
			print '[????] log VOL problem in MI'
			return -1
		if self.LOC() < 1:
			print '[????] log LOC problem in MI'
			return -1
		if not self.vals.mi:
			#self.vals.mi = 0-5.2*math.log(self.getVol())-0.23*self.getMCC()-16.2*math.log(self.getVar()+self.getProd())
			#self.vals.mi = 171 - 5.2*math.log(self.getVol()/(0.0+self.getVar())) - 0.23*self.getMCC() - 16.2*math.log(self.getVar()+self.getProd())
			self.vals.mi = 171 - 5.2*math.log(self.VOL()) - 0.23*self.MCC() - 16.2*math.log(self.LOC())
		return self.vals.mi

#
class TopBench (MeasurementBench,HalsteadBench,PatternBench,CFGBench,CallGraphBench,MiscBench):
	def __init__(self,g):
		# initialise all sub-benches
		EmptyBench.__init__(self,g)
		MeasurementBench.__init__(self,g)
		HalsteadBench.__init__(self,g)
		PatternBench.__init__(self,g)
		CFGBench.__init__(self,g)
		CallGraphBench.__init__(self,g)
		MiscBench.__init__(self,g)
