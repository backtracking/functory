
quick: main-pp.tex  state.mps
	rubber -pdf main-pp
	xpdf -remote tmp -reload

main-pp.pdf: main-pp.tex biblio.bib state.mps
	pdflatex main-pp
	bibtex main-pp
	pdflatex main-pp
	pdflatex main-pp

LATEXPP = latexpp -g color no -e ocaml ocaml-sf -m of ocaml-sf

main-pp.tex: main.tex
	$(LATEXPP) $^ -o $@

state.mps:
	mlpost -latex main.tex figures.ml

export: main-pp.pdf
	cp main-pp.pdf ~/WWW/functory/paper.pdf
