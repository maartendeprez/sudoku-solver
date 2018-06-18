sudoku: sudoku.hs
	ghc -dynamic -threaded -with-rtsopts "-N" -o $@ $<

solve: solve.c
	gcc -O3 -march=native -o $@ $<
