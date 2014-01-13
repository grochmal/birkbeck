#!/usr/bin/env python

__version__   = "0.2"
__author__    = "Michal Grochmal"
__copyright__ = "Michal Grochmal (C) 2014"
__license__   = "GNU General Public License, version 3 or later"
__date__      = "2014-01-12"
__doc__ = """
pgrank - compute normalised PageRank for a distributed graph

A MapReduce program that iterates the PageRank algorithm over nodes and edges
of a distributed graph, outputs results normalised towards `_unity'.  This
implementation uses the "stripes" technique for data ordering.

Floating point precision
------------------------

The probability distribution of the PageRank algorithm shall be initialised
to 1/(number of nodes) so the sum of the page rank value of all nodes will
equal 1.  But because of floating point precision errors this is inviable
when the number of nodes is big.  For 75879 the value of initial page rank
at each node would be 1/75879 = 1.318*10^-5 which, when divided to go over
outlinks can easily hit precision problems.  Moreover when we consider a case
where we have only one dangling node and we need to reassign the probability
mass lost by the lack of an outlink we need to divide the current page rank
of this node by 75879.  On the first iteration it would mean
1.318*10^-5/75879 = 1.737*10^-10 which is way too small to depend on it's
precision.

Therefore instead of assigning 1/N initial page rank to each node we assign
an initial page rank of 100 and consider that 75879*10^2 is the unity in the
probability distribution.  i.e. all results need to be divided by 75879*10^2
to obtain the normalised page rank value of the result.

Stop criterion
--------------

We stop page rank after 30 iterations as it must have converged.  If Google
can converge 322*10^6 edges in 52 iterations [1] our 5*10^5 edges must have
converged after 30.  Just in case the problem was tested with 50 iterations
and the results do not change between 30 or 50 iterations.

References:
  [1] Jimmy Lin, Chris Dyer, "Data-Intensive Text Processing with MapReduce"
      Morgan and Claypool, 2010, pp. 101-107
"""

from mrjob.job import MRJob
import mrjob.protocol
import re

_nodes = 75879
_offset = 100.0
_unity = _offset * _nodes  # we need to normalise against this
_damp = 0.90
_random_jump = (1 - _damp) * _unity / _nodes
_empty = re.compile("^\s*$|^\s*#")
_iterations = 30  # number of times to repeat the PageRank algorithm
_querysize = 10   # number of top results to return

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

class PageRank(MRJob):
    """Definition of our MapReduce jobs using the mrjob module"""

    # with this we can read key value pairs directly from the input file
    INPUT_PROTOCOL = mrjob.protocol.RawProtocol
    # it's the default, but make sure as we need JSON lists to be passed
    INTERNAL_PROTOCOL = mrjob.protocol.JSONProtocol
    # accumulator for the probability mass lost in nodes without outlinks
    mass_loss = 0.0
    # accumulator for the results in the last reducer
    wdw = [(0,None) for i in range(_querysize)]

    def mapper_base(self, person, trust):
        """Outputs all edges A -> B as A -> [B] and B -> [] (nothing).  This is
        needed to be able to find all nodes without outlinks.  The nodes
        without outlinks need to be assigned some initial page rank if we want
        a normalised page rank value in the results.
        """
        if _empty.match(person): return
        yield person, [ trust ]
        yield trust, []

    def combiner_base(self, person, trust_lists):
        """Combines all edges into the form A -> [B,C,D,...].  We use a
        dictionary to ensure that even if the original file has duplicate
        edges we do not pass them further.
        """
        outlinks = {}
        for trusts in trust_lists:
            for t in trusts: outlinks[t] = 1
        yield person, outlinks.keys()

    def reducer_base(self, person, trust_lists):
        """As the combiner, joins all outlinks into a dictionary, preventing
        duplicates.  Then assigns the initial page rank value to all nodes.
        """
        outlinks = {}
        for trusts in trust_lists:
            for t in trusts: outlinks[t] = 1
        yield person, ('outlinks', _offset, outlinks.keys())

    def mapper_pg(self, person, graph):
        """Maintains the graph structure by outputting the values starting with
        'outlinks'.  For nodes without outlinks yields a 'loss' value that will
        be processed in the next MapReduce job.  And for nodes with outlinks it
        divides the node's page rank and gives it equally to each outlink.
        """
        yield person, (graph[0], graph[2])
        if 0 == len(graph[2]):
           yield person, ('loss', graph[1])
        else:
           give_pg = graph[1]/len(graph[2])
           for ol in graph[2]: yield ol, ('pagerank', give_pg)

    def combiner_pg(self, person, values):
        """Sums 'pagerank' pairs, ignores (pass onwards) 'loss' (lost
        probability mass) and 'outlinks' (graph structure) pairs.
        """
        rank = 0.0
        outlinks = []
        for v in values:
            if   'pagerank' == v[0]: rank += v[1]
            elif 'loss'     == v[0]: yield person, v
            elif 'outlinks' == v[0]: yield person, v
        yield person, ('pagerank', rank)

    def reducer_pg(self, person, values):
        """Sums the page rank values and constructs the original pairs but with
        a prefix of 'node'.  The 'loss' pairs are prefixed with 'loss' and
        passed forward.

        The random jump and dampening is accounted for in here.
        """
        rank = 0.0
        outlinks = []
        for v in values:
            if   'pagerank' == v[0]: rank += v[1]
            elif 'loss'     == v[0]: yield ('loss', person), v[1]
            elif 'outlinks' == v[0]: outlinks = v[1]
        yield ('node', person), (_random_jump + _damp*rank, outlinks)

    def mapper_loss(self, person, graph):
        """Identity mapper run just in case to ensure the sorting of keys to
        the following reducer.
        """
        yield person, graph

    def reducer_init_loss(self):
        """Just in case set the mass loss to zero as we cannot be sure of the
        state of the object at each iteration.
        """
        self.mass_loss = 0.0

    def reducer_loss(self, person, values):
        """This reducer accounts for redistributing the lost mass to all nodes,
        therefore it needs to see all nodes.  i.e. only one can run.

        As the keys are ordered all 'loss' keys containing values of mass
        lost in the previous job come first to the reducer.  We sum all these.
        Later the 'node' keys come and for each node we can redistribute the
        mass lost in the parallel phase.  The random jump was accounted for in
        the  parallel phase therefore there's no need to add it here.

        Output keys are formatted to be fed back to the parallel phase.
        """
        if 'loss' == person[0]:
            for v in values: self.mass_loss += v
        else:
            mass_re = self.mass_loss / _nodes
            for v in values: yield person[1], ('outlinks', v[0]+mass_re, v[1])

    def mapper_query(self, person, outlinks):
        """Force all pairs into a single reducer"""
        yield 1, (person, outlinks[1])

    def reducer_query(self, _, values):
        """Runs through all values and keeps the higher `_querysize' results
        inside `self.wdw'.
        """
        for val in values:
            person, rank = val
            self.wdw = sliding_max(self.wdw, (rank, person))

    def reducer_final_query(self):
        """After evaluating all values we have the higher `_querysize' results,
        output those here.
        """
        for r,p in self.wdw: yield p,r

    def steps(self):
        """The EMR configuration for this task can be very different depending
        on how many nodes we need to process.  Yet for the task at hand
        (epinions social network) the most significant configuration is a
        cluster with one m1.small master and 6 m1.large slaves.  The map and
        reduce tasks are divided according to this configuration.

        This task needs an initial phase that prepares the graph to be
        processed with the PageRank algorithm (in `begin'), an iterative phase
        of two MapRaduce jobs (in `pagerank') and a final phase that queries
        the PageRank results (in `query')
        """
        pagerank = reduce( lambda x,y: x+y
                         , [[ self.mr( mapper=self.mapper_pg
                                     , combiner=self.combiner_pg
                                     , reducer=self.reducer_pg
                                     , jobconf=
                                         { 'mapred.job.name' : 'pgrank_stripes'
                                         , 'mapred.map.tasks' : '24'
                                         , 'mapred.reduce.tasks' : '12'
                                         }
                                         # This is the parallel phase of
                                         # PageRank.
                                         #
                                         # With 6 machines the number of
                                         # mappers can be rather big.  But less
                                         # reducers to spread the pairs over
                                         # them.
                                     )
                            , self.mr( mapper=self.mapper_loss
                                     , reducer_init=self.reducer_init_loss
                                     , reducer=self.reducer_loss
                                     , jobconf=
                                         { 'mapred.job.name' : 'pgrank_stripes'
                                         , 'mapred.map.tasks' : '24'
                                         , 'mapred.reduce.tasks' : '1'
                                         }
                                         # This is the phase of PageRank where
                                         # we redistribute the mass lost from
                                         # nodes without outlinks.  The reducer
                                         # must be one as it must see all lost
                                         # mass to sum it, and all nodes to
                                         # redistribute the lost mass.  The
                                         # mappers are only present to enforce
                                         # the sorting of pairs.
                                     )
                            ] for i in range(_iterations)])
        begin = [ self.mr( mapper=self.mapper_base
                         , combiner=self.combiner_base
                         , reducer=self.reducer_base
                         , jobconf=
                             { 'mapred.job.name' : 'pgrank_stripes'
                             , 'mapred.map.tasks' : '24'
                             , 'mapred.reduce.tasks' : '12'
                             }
                             # A big number of mappers to read the input file
                             # (the input file is big).  And a smaller number
                             # of reducers as, hopefully, combiners will shrink
                             # the number of key value pairs.
                         )]
        query = [ self.mr( mapper=self.mapper_query
                         , reducer=self.reducer_query
                         , reducer_final=self.reducer_final_query
                         , jobconf=
                             { 'mapred.job.name' : 'pgrank_stripes'
                             , 'mapred.map.tasks' : '12'
                             , 'mapred.reduce.tasks' : '1'
                             }
                             # The amount of map tasks can be big to process
                             # the pairs and send all of them to the one
                             # reducer.  The reducer must be one as it must
                             # see all results.
                         )]
        return begin + pagerank + query

if '__main__' == __name__:
    PageRank.run()

