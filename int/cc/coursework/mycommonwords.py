#!/usr/bin/env python

from mrjob.job import MRJob
import re

WORD_RE = re.compile(r"[\w']+")

class MRMostUsedWord(MRJob):
    maxwords = [(0,'') for i in range(20)]

    def mapper_get_words(self, _, line):
        # yield each word in the line
        for word in line.split():
            yield (word.lower(), 1)

    def combiner_count_words(self, word, counts):
        # optimization: sum the words we've seen so far
        yield (word, sum(counts))

    def reducer_count_words(self, word, counts):
        # send all (num_occurrences, word) pairs to the same reducer.
        # num_occurrences is so we can easily use Python's max() function.
        yield None, (sum(counts), word)

    def mapper_find_max_word(self, _, word_count_pair):
        for i in range(len(self.maxwords)):
            if self.maxwords[i][0] < word_count_pair[0]:
                self.maxwords[i] = word_count_pair
                break
        # each item of word_count_pairs is (count, word),
        # so yielding one results in key=counts, value=word
    def mapper_final(self):
        for c,w in self.maxwords:
            yield c,w

    def steps(self):
        return [ self.mr(mapper=self.mapper_get_words,
                         combiner=self.combiner_count_words,
                         reducer=self.reducer_count_words),
                 self.mr(mapper=self.mapper_find_max_word,
                         mapper_final=self.mapper_final)
               ]

if __name__ == '__main__':
    MRMostUsedWord.run()

