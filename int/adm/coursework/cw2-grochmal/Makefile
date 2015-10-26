# Makefile for the 2nd ADM coursework

LATEX  = pdflatex
DOT    = dot
DOTFRM = -T
DOTOUT = -o
RM     = rm -f
LTFS   = coursework.aux coursework.out coursework.log coursework.idx \
         coursework.ilg coursework.ind coursework.pdf
IMGS   = queryA.pdf queryB.pdf queryC.pdf queryD.pdf

all: coursework.pdf

coursework.pdf: coursework.tex $(IMGS)
	$(LATEX) $<
	$(LATEX) $<

%.pdf: %.dot
	$(DOT) $(DOTFRM) pdf $(DOTOUT) $@ $<

.PHONY: clean
clean:
	$(RM) $(LTFS)
	$(RM) $(IMGS)

