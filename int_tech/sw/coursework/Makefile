# Makefile for SW coursework

LATEX  = pdflatex
DOT    = dot
DOTFRM = -T
DOTOUT = -o
RM     = rm -f
LTFS   = coursework.aux coursework.out coursework.log coursework.idx \
         coursework.ilg coursework.ind coursework.pdf
IMGS   = ex1_playwrights.pdf ex2_bob.pdf ex4_dwarf.pdf \
         ex7_interpretations.pdf ex8_ron.pdf ex9_dc.pdf

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

