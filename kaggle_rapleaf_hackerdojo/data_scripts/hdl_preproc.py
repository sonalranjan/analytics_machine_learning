#!/usr/bin/env python
import sys
import os
import csv


# users that are in training set
hdl = csv.reader(open("headlines.tsv"), delimiter="\t")
hdl_hdr = hdl.next()

print ','.join([ hdl_hdr[0], hdl_hdr[3], hdl_hdr[1], "url_1", "url_2", "url_n" ])

for h in hdl:
    ### if h[1] == '': continue 
    a = [h[0], h[3], h[1]]
    b = h[1].split('/')
    if '' in b: b.remove('') # remove the first ''
    while len(b) < 3: b.append('')
    a.extend([b[0], b[1], b[-1]])
    print ','.join(a)

