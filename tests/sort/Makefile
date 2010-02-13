
all: a.out file-1_000-5 file-1_000_000-5

file-%-5:
	ocaml bench.ml $* 5 > $@

a.out:
	make -C ../.. tests/sort/a.out

