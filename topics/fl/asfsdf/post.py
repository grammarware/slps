#!/usr/bin/python
import sys
for line in sys.stdin:
 print line.strip().split(';')[0].strip()
