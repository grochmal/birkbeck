
LATEX  = pdflatex
DOT    = dot
DOTFRM = -T
DOTOUT = -o
RM     = rm -f
EPS    = epstopdf
EPSOUT = -o
LTFS   = coursework.aux coursework.out coursework.log \
         coursework.idx coursework.ilg coursework.ind
IMGS   = ex1_playwrights.pdf

all: coursework.pdf

coursework.pdf: coursework.tex $(IMGS)
	$(LATEX) $<
	$(LATEX) $<

%.pdf: %.dot
	$(DOT) $(DOTFRM) pdf $(DOTOUT) $@ $<

%.pdf: %.eps
	$(EPS) $(EPSOUT) $@ $<

.PHONY: clean
clean:
	$(RM) $(LTFS)
	$(RM) *.pdf

