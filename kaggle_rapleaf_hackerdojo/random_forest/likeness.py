#!/usr/bin/env python
import sys
import os
import csv
import gzip

def my_open(fname):
    if True == fname.endswith(".gz"): return gzip.open(fname)
    return open(fname)

gold = {}
t = csv.reader(my_open(sys.argv[1]))
hdr = t.next()
for h in t: gold[h[0]] = float(h[1])

for f in sys.argv[2:]:
    dist_sq = 0
    disag = 0
    crap = 0
    t = csv.reader(my_open(f))
    hdr = t.next()
    for h in t:
        if not h[0] in gold: 
            crap += 1
            continue
        v = float(h[1])
        dist_sq += (v-gold[h[0]])**2
        if (v-0.5)*(gold[h[0]]-0.5) < 0 : disag += 1
    print "%s: %d %g"%(f, disag, (dist_sq)**0.5 )
    print "%s: crap=%d"%(f, crap)
