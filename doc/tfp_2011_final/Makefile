
quick: main-pp.tex biblio.bib state.mps
	rubber -d main-pp

LATEXPP = latexpp -g color no -e ocaml ocaml-sf -m of ocaml-sf

main-pp.tex: main.tex
	$(LATEXPP) $^ -o $@

state.mps: figures.ml
	TEXINPUTS=`pwd`: mlpost -latex main.tex figures.ml

export: main-pp.pdf
	cp main-pp.pdf ~/WWW/publis/tfp11.pdf
