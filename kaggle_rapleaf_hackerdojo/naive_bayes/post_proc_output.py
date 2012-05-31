#!/usr/bin/env python
import sys
import os
import csv

u_hash = {}

wt = 0.5

t3 = csv.reader(open(sys.argv[1]))
for h in t3:
    hkey = int(float(h[0]))
    a = u_hash.get( hkey, (0.0,0.0,0))
    u_hash[hkey] = ( a[0] + float(h[1]), a[1] + float(h[2]), a[2] + 1)

### print len(u_hash.keys()) 

for k in sorted(u_hash.keys()):
    t = u_hash[k]
    zo = t[1]/t[2] 
    #if zo > wt: zo = 1 else: zo = 0
    ### print "%d,%s"%(k,u_hash[k]) 
    print "%d,%.2f"%(k,zo)
