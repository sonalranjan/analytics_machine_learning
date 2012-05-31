#!/usr/bin/env python
import sys
import os
import csv

u_hash = {}
global_class_arr = []
count = 0

rdr_msg1 = csv.reader( open("preproc_hdl_massaged1.csv"))
rdr_hdr = rdr_msg1.next()
for l in rdr_msg1:
    count += 1
    class_col_name = "url_" + l[1] + "_" + l[2]
    if not class_col_name in global_class_arr:
        global_class_arr.append( class_col_name )
    #if count%100000 == 0: print count


ro_blank_arr = []
global_class_arr.sort()
### print global_class_arr 
for j in global_class_arr:
    ro_blank_arr.append('0')

curr_class_arr = [ "uid" ] + global_class_arr
rdr_msg1 = csv.reader( open("preproc_hdl_massaged1.csv"))
rdr_hdr = rdr_msg1.next()
last_uid = None
for l in rdr_msg1:
    if last_uid != l[0]:
        print ",".join(curr_class_arr)
        last_uid = l[0]
        curr_class_arr = ro_blank_arr
        curr_class_arr = [ l[0] ] + curr_class_arr
    class_col_name = "url_" + l[1] + "_" + l[2]
    if class_col_name in global_class_arr:
        idx = global_class_arr.index(class_col_name) + 1
        curr_class_arr[idx] = '1'
print ",".join(curr_class_arr)
