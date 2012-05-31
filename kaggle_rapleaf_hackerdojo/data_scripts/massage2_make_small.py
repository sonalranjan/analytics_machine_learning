#!/usr/bin/env python
import sys
import os
import csv

all_small_uids = {}
rdr_csv = csv.reader( open("small_ce_demog.csv") )
for l in rdr_csv:
    all_small_uids[l[0]] = 1

rdr_csv = csv.reader( open("preproc_hdl_massaged2.csv"))
for l in rdr_csv:
    if l[0] in all_small_uids:
        print ",".join(l)

