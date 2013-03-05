#! /usr/bin/env python
# -*- coding: utf-8 -*-
import sys

f = open('banned.dirs','r')
banned = [x.strip() for x in f.readlines()]
f.close()

for line in sys.stdin.readlines():
	line = line.strip()
	good = True
	for b in banned:
		if line.startswith(b):
			good = False
	if good:
		print line
