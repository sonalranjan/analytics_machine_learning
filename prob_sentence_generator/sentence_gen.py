#!/usr/bin/env python
import sys
import os
import random
import re

############################################################
# problem: given a corpus - generate sentences that are similar to the corpus
#
# solution: probabilistic generator automaton
#
# 1. Tokenize. Populate word universe.
# 2. calculate transition-probabilities
# 3. generate sentences
############################################################

class WordUniverse:
    def __init__(self, preferred_stop_length=10, non_terminals=['is', 'the', 'and', 'or', 'if'], terminals=[]):
        self.tokens = {}
        self.stop_length = preferred_stop_length
        self.non_terminals = non_terminals
        self.terminals = terminals
        self.word_re = re.compile('^[a-z\-]+$',re.IGNORECASE)
        self.word_sep = re.compile('[\s;:,\.()\[\]-]+')
        #self.word_sep = re.compile('[\s;:]+')
        self.automat = {}
        self.automat_prob = {}
        self.wu = {}

    def tokenize(self, fname):
        for l in open(fname):
            l = l.strip()
            words = re.split(self.word_sep, l)
            if '' in words: words.remove('')
            #print words 
            words = [ w for w in words if re.match(self.word_re, w) ]
            #print words, l
            prev_w = None
            for w in words[1:]:
                #print "self.insertword( %s, %s)"%(prev_w, w)
                self.insertword( prev_w, w)
                prev_w = w

    def make_probs(self):
        for w in self.automat:
            sum_sw = sum([ self.automat[w][sw] for sw in self.automat[w].keys()])
            self.automat_prob[w] = []
            cum_prob = 0.0
            for k in self.automat[w].keys():
                self.automat[w][k] = self.automat[w][k]/sum_sw
                cum_prob += self.automat[w][k]
                self.automat_prob[w].append( (cum_prob, k) )

    def insertword(self, prev_word, w):
        # increase word count
        self.wu.setdefault(w, 0)
        self.wu[w] += 1
        # increase count for successors
        if prev_word != None:
            self.automat.setdefault(prev_word, {})
            self.automat[prev_word].setdefault(w, 0.0)
            self.automat[prev_word][w] += 1.0

    def __repr__(self):
        str_ = ""
        for w in self.wu: 
            str_ += "\nword: " + w + " " + str(self.wu[w]) + " " + str(self.automat.get(w, "None"))
        str_ += "\ncum_probs: " + str(self.automat_prob)
        return str_

    def make_sentence(self, start_w, pref_len, debug=False):
        if not start_w in self.automat: return
        sent_ = start_w 
        cur_w = start_w
        len_ = 1
        while len_ < pref_len:
            len_ += 1
            pr = random.uniform(0,1)
            for t in self.automat_prob[cur_w]:
                if debug: print pr, cur_w, t
                if pr <= t[0]: 
                    sent_ += " " + t[1]
                    cur_w = t[1]
                    break
        return sent_



if __name__ == "__main__":
    random.seed(9)
    wuobj = WordUniverse()
    wuobj.tokenize(sys.argv[1])
    # print wuobj
    wuobj.make_probs()
    #print wuobj
    print "\n\n"
    print wuobj.make_sentence('the', 4)
    print wuobj.make_sentence('image', 4)
    print wuobj.make_sentence('it', 10)





