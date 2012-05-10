#!/usr/local/bin/python
# -*- coding: utf-8 -*-
import os, sys, math
import slpsns
import elementtree.ElementTree as ET
import metrics

def bgf2dot(g):
	cg = metrics.getCallGraph(g)
	s = 'digraph generated{\n'
	for a in cg.keys():
		for b in cg[a]:
			s += a+' -> '+b+';\n'
	#s += '}\n'
	print metrics.calculateLevels(cg)
	cg = metrics.makeOneStep(cg)
	#s = 'digraph generated{\n'
	for a in cg.keys():
		s += a.replace('-','_')+'_[label="'+a+'"];\n'
		for b in cg[a]:
			s += a.replace('-','_')+'_ -> '+b+'_;\n'
	print metrics.calculateLevels(cg)
	cg = metrics.makeOneStep(cg)
	for a in cg.keys():
		s += a.replace('-','_')+'__[label="'+a+'"];\n'
		for b in cg[a]:
			s += a.replace('-','_')+'__ -> '+b+'__;\n'
	print metrics.calculateLevels(cg)
	s += '}'
	print metrics.calculateLevels(metrics.getClosure(cg))
	return s
