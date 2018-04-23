PDFLATEX=pdflatex
BIBLATEX=bibtex

clean:
	rm *.aux
	rm *.log
	rm *.bbl
	rm *.blg
	rm *.run.xml

pdf:
	$(PDFLATEX) manual.tex
	xdg-open manual.pdf

all:
	make pdf
	make clean