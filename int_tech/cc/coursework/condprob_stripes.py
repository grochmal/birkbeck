#!/usr/bin/env python

__version__   = "0.3"
__author__    = "Michal Grochmal"
__copyright__ = "Michal Grochmal (C) 2014"
__license__   = "GNU General Public License, version 3 or later"
__date__      = "2014-01-12"
__doc__ = """
condprob - conditional probability of words that appear after a certain word

A MapReduce program that estimates the probability of words appearing after a
word defined in `_queryword'.  Returned are the top `_querysize' results, i.e.
the words with the higher probability of appearing after the given word.

This can be performed on an arbitrarily big corpus.  This implementation uses
the "stripes" technique for data ordering.
"""

from mrjob.job import MRJob
import mrjob.protocol
import re

_rewords = re.compile("[\w']+")
_queryword = 'my'                # word we want to calculate the next word for
_querysize = 10                  # number of top results to return (if less
                                 # results are found returns nulls to fill)
def sliding_max(wdw, val):
    """Given a window of 2-tuples and a 2-tuple value it outputs the window
    containing the max values among the ones in the window and the new value.
    i.e. it puts the value inside the window if it is bigger than at least one
    value currently in the window, otherwise throws the value away.  The
    resulting window have always the same size as the original one and is
    sorted in descending order.  If the 2-tuple value is inserted in the window
    the smaller tuple in the window is thrown away.

    Only the first members of the tuples are compared the second member of any
    tuple can contain any value.
    """
    c,v = val
    for i in range(len(wdw)):
        if wdw[i][0] < c:
            wdw = wdw[:i] + [(c,v)] + wdw[i:-1]
            break;
    return wdw

def sliding_max_test(window_size, values):
    """Test procedure for `sliding_max', sorts `values' into a sliding window
    of `window_size'.  Not used in the execution, it's here for completeness.
    """
    wdw = [(0,None) for i in range(window_size)]
    for val in values: wdw = sliding_max(wdw, val)
    return wdw

class CondProb(MRJob):
    """Definition of our MapReduce jobs using the mrjob module"""

    # it's the default, but make sure as we need JSON objects to be passed
    INTERNAL_PROTOCOL = mrjob.protocol.JSONProtocol
    # accumulator for the paragraphs in the first mapper
    par = []
    # accumulator for the results in the last reducer
    wdw = [(0,None) for i in range(_querysize)]

    def yield_paragraph(self, paragraph):
        """Given a list of lines representing a paragraph yields for each pair
        of words (including pairs between last word of line and first word of
        next line) the first word as a key and the next word as the value.  The
        value is held as a key in a dictionary (actually a JSON object is what
        is output).

        The regular expression ensures that we do not have special characters
        in the words, so we are free to use special characters to join and
        split the words later.  The regular expression plus .lower() also
        ensures the normalisation of the words.
        """
        words = _rewords.findall(' '.join(paragraph))
        words = map(lambda x: x.lower(), words)
        pairs = zip(words, words[1:])
        for p in pairs: yield p[0], { p[1] : 1 }

    def mapper_prob(self, _, line):
        """To divide the file into paragraphs we assume that an empty line ends
        the paragraph.  We accumulate all non empty lines and output all of
        them when an empty one is found.  When more than one empty line follows
        each other there are no words to output so those are skipped.
        """
        line = line.strip()
        if '' != line:
            self.par.append(line)
        elif [] != self.par:
            for y in self.yield_paragraph(self.par): yield y
            self.par = []

    def mapper_final_prob(self):
        """It might happen that there is no empty line after the last
        paragraph, yet we still have that paragraph accumulated.  If so output
        it here.
        """
        if [] != self.par:
            for y in self.yield_paragraph(self.par): yield y

    def combiner_prob(self, word, values):
        """The dictionaries of next words for a given word can be freely
        combined by summing the values in those dictionaries by key.
        """
        combined = {}
        for val in values:
            for w in val: combined[w] = combined.get(w, 0) + val[w]
        yield word, combined

    def reducer_prob(self, kword, values):
        """Once again the dictionaries are summed by key.  Once that is done we
        can estimate P(kword) by summing all keys.  Each key can be seen as the
        estimate of P(kword,w) so we make a simple division to achieve
        P(w|kword).  Which is written out.
        """
        combined = {}
        for val in values:
            for w in val: combined[w] = combined.get(w, 0) + val[w]
        denom = sum([combined[w] for w in combined])
        for w in combined: yield w+'|'+kword, float(combined[w])/float(denom)

    def mapper_query(self, words, prob):
        """Divide the key once again into words and forces all pairs into one
        reducer by assigning everything to a single key.
        """
        w1,w2 = words.split('|')
        yield 1, (w2, w1, prob)

    def reducer_query(self, _, values):
        """Runs through all keys and keeps the higher `_querysize' results
        inside `self.wdw'.  Only values that are derived from `_queryword' are
        processed through the comparison.
        """
        for val in values:
            w2,w1,prob = val
            if w2 != _queryword: continue
            self.wdw = sliding_max(self.wdw, (prob, w1+'|'+w2))

    def reducer_final_query(self):
        """After evaluating all values we have the higher `_querysize' results,
        output those here.
        """
        for c,w in self.wdw: yield w,c

    def steps(self):
        """This is a rather small job, so it is expected to run on EMR on three
        m1.small instances.  The time to run it can be shortened by using
        bigger instances but using more machines will hider the performance as
        the time to start machines is comparable to the time to run the tasks.
        """
        return [ self.mr( mapper=self.mapper_prob
                        , mapper_final=self.mapper_final_prob
                        , combiner=self.combiner_prob
                        , reducer=self.reducer_prob
                        , jobconf=
                            { 'mapred.job.name' : 'condprob_stripes'
                            , 'mapred.map.tasks' : '8'
                            , 'mapred.reduce.tasks' : '3'
                            # 8 map tasks are used as we have 8 files in the
                            # works of Jane Austen.  In the optimal case each
                            # file is given to on mapper, as otherwise
                            # paragraphs may be divided in the middle during
                            # the split of the input.  Unfortunately the
                            # optimal case is hardly true and some paragraphs
                            # will end chopped in the middle.  Fortunately the
                            # error from chopping the paragraphs is not big.
                            #
                            # We use less reducers than mappers so the map
                            # outputs can be spread over the reducers.
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
                            # The amount of map tasks can be big to process the
                            # pairs and send all of them to the one reducer.
                            # The reducer must be one as it must see all
                            # results.
                        )
               ]

if '__main__' == __name__:
    CondProb.run()

