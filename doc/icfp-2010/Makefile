
quick: main-pp.tex
	pdflatex main-pp
	xpdf -remote tmp -reload

main-pp.pdf: main-pp.tex biblio.bib
	pdflatex main-pp
	bibtex main-pp
	pdflatex main-pp
	pdflatex main-pp

LATEXPP = latexpp -g color no -e ocaml ocaml-sf -m of ocaml-sf

main-pp.tex: main.tex
	$(LATEXPP) $^ -o $@
