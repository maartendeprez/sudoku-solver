all: solve sudoku

sudoku: sudoku.hs Makefile
	ghc -dynamic -threaded -with-rtsopts "-N" -o $@ $<

solve: solve.c Makefile
	gcc -O3 -march=native -o $@ $< # -g -pg
