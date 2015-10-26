#!/usr/bin/env python

from mrjob.job import MRJob
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
    par = ''
    wdw = [(0,None) for i in range(_querysize)]
    SORT_VALUES = True

    def yield_paragraph(self, p):
        words = _rewords.findall(p)
        words = map(lambda x: x.lower(), words)
        lenws = len(words)
        for i in range(lenws):
            yield words[i], '1sngl:1:'
            if i < lenws-1: yield words[i], '2pair:1:'+words[i+1]

    def mapper_prob(self, _, line):
        line = line.strip()
        if '' != line:
            if '' != self.par: self.par += ' '
            self.par += line
        elif '' != self.par:
            for y in self.yield_paragraph(self.par): yield y
            self.par = ''

    def mapper_final_prob(self):
        if '' != self.par:
            for y in self.yield_paragraph(self.par): yield y

    def combiner_prob(self, word, values):
        singles = 0
        pairs = {}
        for val in values:
            typ,count,nextw = val.split(':')
            if '1sngl' == typ : singles += int(count)
            else              : pairs[nextw] = pairs.get(nextw,0) + int(count)
        yield word, '1sngl:'+str(singles)+':'
        for nw in pairs: yield word, '2pair:'+str(pairs[nw])+':'+nw

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
        return [ self.mr(mapper=self.mapper_prob,
                         mapper_final=self.mapper_final_prob,
                         combiner=self.combiner_prob,
                         reducer=self.reducer_prob),
                 self.mr(mapper=self.mapper_query,
                         reducer=self.reducer_query,
                         reducer_final=self.reducer_final_query)
               ]
if '__main__' == __name__:
    CondProb.run()

