sudoku: sudoku.hs
	ghc -dynamic -threaded -with-rtsopts "-N" -o $@ $<
