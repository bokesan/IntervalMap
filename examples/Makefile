.SUFFIXES: .lhs .mkd .html .tex .pdf

PANDOC := pandoc --no-wrap -sS
HSCOLOUR := HsColour -lit

.lhs.mkd:
	cat $< | $(HSCOLOUR) -css > $@

.lhs.html:
	cat $< | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css > $@

.lhs.tex:
	cat $< | $(HSCOLOUR) -latex | $(PANDOC) -t latex> $@

.tex.pdf:
	pdflatex $< && pdflatex $< && pdflatex $<

