#!/usr/bin/env python
import sys
import os
import csv
import re


##################################################
# a utility to preprocess demography.tsv data
# python demog.py  [ in the same directory as demography.tsv ]
# 
# what does this do ?
#
# converts:
# 12696087	35-44	Female	Long Valley, New Jersey, United States	Yes	300k-350k	Own	Single Family Dwelling	150k-175k	10 Years	Married
# to:
# 12696087,39.5,Female,Long_Valley,New_Jersey,United_States,Yes,325000.0,Own,Single_Family_Dwelling,162500.0,10.0,Married
#
# 1. split location into city/state/country
# 2. split numeric ranges, multiplies with suffix (k,mm)  [ you can modify the
# code for numeric range handling to output something other than average ]
# 
##################################################

class Preproc:

    def __init__(self, NA_shadow=False):
        self.NA_shadow=NA_shadow
        self.num_re = re.compile('\s*[.\d]+\s*')
        self.yr_sfx_re = re.compile('\s+year.*', re.IGNORECASE)
        self.fld_arr = [ "uid","age","gender","location","children","home_market_value","home_owner_status","home_property_type","household_income","length_of_residence","marital_status"]
        self.preproc_hash = {
            "age" : self.num_range_preproc,
            "location" : self.loc_preproc,
            "home_market_value" : self.num_range_preproc,
            "home_property_type" : self.str_preproc,
            "household_income" : self.num_range_preproc,
            "length_of_residence" : self.lor_preproc,
            }
        # do some special handling for these fields
        self.fld_proc = [ "age", "location", "home_market_value", "home_property_type", "household_income", "length_of_residence" ]
        self.fidx_proc = [ self.fld_arr.index(f) for f in self.fld_proc ]
        # elements that may need a 2nd pass of processing. [ example: location -> (city, state, country) will need flattening. "home_property_type" is for testing ref:index_bumping ]
        if True == NA_shadow:
            self.fidx_proc2 = sorted([ self.fld_arr.index(f) for f in [ "age", "home_market_value", 'household_income', 'length_of_residence', "location", "home_property_type" ]])
        else:
            self.fidx_proc2 = [ self.fld_arr.index(f) for f in [ "location", "home_property_type" ]]

    def num_sfx_preproc(self, n):
        if '+' in n: n = n[0:n.index('+')] # strip '+'
        # convert sfx to a multiplier
        if 'k' in n: n = int( n[0:n.index('k')])*1000.
        elif 'm' in n: n = int( n[0:n.index('m')])*1000.
        return float(n)

    def NA_shadow_proc(self, t):
        if True == self.NA_shadow: 
            if len(str(t)) > 0: return [t,"0"]
            else: return [t,"1"]
        else: return t

    def num_range_preproc(self, txt):
        if len(txt) == 0: return self.NA_shadow_proc( txt)
        t = txt.split('-')
        try:
            t = [ self.num_sfx_preproc(f) for f in t ]
            return self.NA_shadow_proc( str(sum(t)/len(t)) )
        except:
            ### print "exception",t 
            return self.NA_shadow_proc( txt)

    def loc_preproc(self, txt):
        if len(txt) == 0: return ['','','']
        t = txt.split(',')
        while len(t) < 3: t.insert(0, '') # insert blanks for city/state/country if not provided
        t = [ self.str_preproc(f) for f in t ]
        if len(t) > 3:
            t = [ '_'.join(t[0:-2]), t[-2], t[-1] ]
        return t

    def lor_preproc(self, txt):
        t = re.sub(self.yr_sfx_re, '', txt.strip())
        t = self.num_range_preproc(t)
        return t

    def str_preproc(self, txt):
        if len(txt) == 0: return txt
        t = '_'.join(txt.split())
        return t

    def preproc_hdr(self, ltxt):
        alt_demog_hdr = ltxt
        add_for_these_cols = { 
                'age' : ['age','age_NA'], 
                'home_market_value' : ['home_market_value', 'home_market_value_NA'], 
                'household_income' : ['household_income', 'household_income_NA'], 
                'length_of_residence' : ['length_of_residence', 'length_of_residence_NA'],
                'location' : ['addr_city', 'state', 'country'],
                }
        #
        l_add = 0
        for fidx in self.fidx_proc2:
            colname = self.fld_arr[fidx]
            if not (colname in add_for_these_cols): 
                continue
            fidx += l_add
            #print "continue for " + colname
            l_add += (len(add_for_these_cols[colname])-1)
            for l in reversed(add_for_these_cols[colname]):
                alt_demog_hdr.insert(fidx+1, l)
            del alt_demog_hdr[fidx]
        return alt_demog_hdr

    def preproc(self, ltxt):
        # make a copy of orig line
        line_text = list(ltxt)
        # preprocess those fields that we have marked for preprocessing
        for fidx in self.fidx_proc:
            app_f = self.preproc_hash.get(self.fld_arr[fidx], None)
            if None != app_f:
                line_text[fidx] = app_f(line_text[fidx])
        # we may need to do a 2nd-preprocess pass (interpolate/flatten lists. for example, location -> [city, state, country])
        l_add = 0
        for fidx in self.fidx_proc2:
            fidx += l_add
            ### print "interp: ", line_text[fidx]  
            if type(line_text[fidx]) == list:
                # flattening willl change list size. bump up indices of things to yet to come [ ref:index_bumping ]
                l_add += (len(line_text[fidx])-1)
                for l in reversed(line_text[fidx]):
                    line_text.insert(fidx+1, l)
                del line_text[fidx]
        return line_text

if __name__ == "__main__":
    ppObj = Preproc(NA_shadow=True)
    demog = csv.reader(open("demographics.tsv"), delimiter="\t")
    demog_hdr = demog.next()

    alt_demog_hdr = ppObj.preproc_hdr(demog_hdr)
    print ','.join(alt_demog_hdr)

    for l in demog:
        lproc = ppObj.preproc(l)
        ### print l, lproc   
        print ','.join(lproc)

