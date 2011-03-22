
fr-slides-pp.pdf: fr-slides-pp.tex state.mps
	pdflatex fr-slides-pp

slides-pp.pdf: slides-pp.tex state.mps
	pdflatex slides-pp
	grep -iw frametitle slides.tex | wc -l

state.mps: figures.ml
	mlpost -latex slides.tex figures.ml

%-pp.tex: %.tex
	latexpp $^ -o $@

