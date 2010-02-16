
slides-pp.pdf: slides.tex
	latexpp slides.tex -o slides-pp.tex
	pdflatex slides-pp
	xpdf -remote tmp -reload
	grep -iw frametitle slides.tex | wc -l
