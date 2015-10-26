#!/usr/bin/env python

from mrjob.job import MRJob
import mrjob.protocol
import re

_empty = re.compile("^\s*$|^\s*#")
_queryword = 'my'
_querysize = 10

def sliding_max(wdw, val):
    c,v = val
    for i in range(len(wdw)):
        if wdw[i][0] < c:
            wdw = wdw[:i] + [(c,v)] + wdw[i:-1]
            break;
    return wdw

def sliding_max_test(window_size, values):
    wdw = [(0,None) for i in range(window_size)]
    for val in values: wdw = sliding_max(wdw, val)
    return wdw

class CondProb(MRJob):
    par = ''
    wdw = [(0,None) for i in range(_querysize)]
    INPUT_PROTOCOL = mrjob.protocol.RawProtocol
    INTERNAL_PROTOCOL = mrjob.protocol.JSONProtocol
    SORT_VALUES = True
    JOBCONF = { 'mapred.map.tasks' : 6
              , 'mapred.reduce.tasks' : 3
              }

    def mapper_base(self, person, trust):
        if _empty.match(person): return
        yield person, [trust]

    def reducer_base(self, person, trust_lists):
        everyone = {}  # just in case, prevent duplicates
        for trusts in trust_lists:
            for t in trusts: everyone[t] = 1
        yield person, everyone.keys()

    def reducer_prob(self, word, values):
        count_sngl = 0.0001  # just in case, prevent division by zero
        pairs = {}
        for val in values:
            typ,count,nextw = val.split(':')
            if '1sngl' == typ : count_sngl += int(count)
            elif '' != nextw  : pairs[nextw] = pairs.get(nextw,0) + int(count)
            else              : yield '!|'+word, 'ERROR:['+word+'|'+val+']'
        for nw in pairs: yield nw+'|'+word, str(pairs[nw]/count_sngl)

    def mapper_query(self, words, prob):
        w1,w2 = words.split('|')
        yield 1, w2+':'+w1+':'+prob

    def reducer_query(self, _, values):
        for val in values:
            w2,w1,prob = val.split(':')
            if w2 != _queryword: continue
            self.wdw = sliding_max(self.wdw, (prob, w1+'|'+w2))

    def reducer_final_query(self):
        for c,w in self.wdw: yield w,c

    def steps(self):
        return [ self.mr(mapper=self.mapper_base,
                         combiner=self.reducer_base,
                         reducer=self.reducer_base)
                 #self.mr(mapper=self.mapper_query,
                         #reducer=self.reducer_query,
                         #reducer_final=self.reducer_final_query)
               ]
if '__main__' == __name__:
    CondProb.run()

