PDFLATEX=pdflatex
BIBLATEX=bibtex

clean:
	rm *.aux
	rm *.log

pdf:
	$(PDFLATEX) manual.tex
	xdg-open manual.pdf

all:
	make pdf
	make clean
