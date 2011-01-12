
slides-pp.pdf: slides-pp.tex state.mps
	pdflatex slides-pp
	grep -iw frametitle slides.tex | wc -l

state.mps: figures.ml
	mlpost -latex slides.tex figures.ml

slides-pp.tex: slides.tex
	latexpp slides.tex -o slides-pp.tex
