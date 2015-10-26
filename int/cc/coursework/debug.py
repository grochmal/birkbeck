#!/usr/bin/env python

from mrjob.job import MRJob
import re

rewords = re.compile("[\w']+")

class MRDebugRunner(MRJob):
    PARTITIONER = 'org.apache.hadoop.mapred.lib.KeyFieldBasedPartitioner'

    def mapper_dbg(self, _, line):
        if '' == line: return
        words = rewords.findall(line)
        pairs = zip(words, words[1:])
        for w in words: yield w+':1sngl', 1
        for p in pairs: yield p[0]+':2pair', p

    def reducer_dbg(self, key, values):
        kword,typ = key.split(':')
        if   '1sngl' == typ: self.den = float(sum(values))
        elif '2pair' == typ:
            word_count = {}
            for v in values: word_count[v[1]] = word_count.get(v[1], 0) + 1
            for w in word_count: yield w+'|'+kword, word_count[w]/self.den

    def reducer_dbgs(self, key, values):
        self.den = 0.001
        word_count = {}
        for val in values:
            typ,count,word = val.split(':')
            if   '1sngl' == typ: self.den += int(count)
            elif '2pair' == typ:
                word_count[word] = word_count.get(word, 0) + int(count)
        for w in word_count: yield w+'|'+key, word_count[w]/self.den

    def steps(self):
        return [ self.mr( mapper=self.mapper_dbg
                        , reducer=self.reducer_dbg
                        , jobconf=
                            { 'mapred.job.name' : 'debugrun'
                            , 'mapred.map.tasks' : '3'
                            , 'mapred.reduce.tasks' : '3'
                            # The options below do not work on amazon
                            # because of mythical reasons
                            #, 'map.output.key.field.separator' : ':'
                            #, 'mapred.text.key.partitioner.options' : '-k1'
                            #, 'mapred.output.key.comparator.class' :
                        #'org.apache.hadoop.mapred.lib.KeyFieldBasedComparator'
                            #, 'mapred.text.key.comparator.options' : '-k2'
                            #, 'mapred.task.timeout' : '60000'
                            #, 'mapreduce.task.timeout' : '60000'
                            })
               ]

if '__main__' == __name__:
    MRDebugRunner.run()

