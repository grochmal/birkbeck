#!/usr/bin/env python

from mrjob.job import MRJob
import mrjob.protocol
import re

_rewords = re.compile("[\w']+")
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
    INTERNAL_PROTOCOL = mrjob.protocol.JSONProtocol
    par = []
    wdw = [(0,None) for i in range(_querysize)]

    def yield_paragraph(self, paragraph):
        words = _rewords.findall(' '.join(paragraph))
        words = map(lambda x: x.lower(), words)
        pairs = zip(words, words[1:])
        for p in pairs: yield p[0], { p[1] : 1 }

    def mapper_prob(self, _, line):
        line = line.strip()
        if '' != line:
            self.par.append(line)
        elif [] != self.par:
            for y in self.yield_paragraph(self.par): yield y
            self.par = []

    def mapper_final_prob(self):
        if [] != self.par:
            for y in self.yield_paragraph(self.par): yield y

    def combiner_prob(self, word, values):
        combined = {}
        for val in values:
            for w in val: combined[w] = combined.get(w, 0) + val[w]
        yield word, combined

    def reducer_prob(self, kword, values):
        combined = {}
        for val in values:
            for w in val: combined[w] = combined.get(w, 0) + val[w]
        denom = sum([combined[w] for w in combined])
        for w in combined: yield w+'|'+kword, float(combined[w])/float(denom)

    def mapper_query(self, words, prob):
        w1,w2 = words.split('|')
        yield 1, (w2, w1, prob)

    def reducer_query(self, _, values):
        for val in values:
            w2,w1,prob = val
            if w2 != _queryword: continue
            self.wdw = sliding_max(self.wdw, (prob, w1+'|'+w2))

    def reducer_final_query(self):
        for c,w in self.wdw: yield w,c

    def steps(self):
        return [ self.mr( mapper=self.mapper_prob
                        , mapper_final=self.mapper_final_prob
                        , combiner=self.combiner_prob
                        , reducer=self.reducer_prob
                        , jobconf=
                            { 'mapred.job.name' : 'condprob_stripes'
                            , 'mapred.map.tasks' : '8'  # 8 files
                            , 'mapred.reduce.tasks' : '3'
                            }
                        )
               , self.mr( mapper=self.mapper_query
                        , reducer=self.reducer_query
                        , reducer_final=self.reducer_final_query
                        , jobconf=
                            { 'mapred.job.name' : 'condprob_stripes'
                            , 'mapred.map.tasks' : '6'
                            , 'mapred.reduce.tasks' : '1'
                            }
                        )
               ]
if '__main__' == __name__:
    CondProb.run()

