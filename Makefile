all: solve solve-acc sudoku

sudoku: sudoku.hs Makefile
	ghc -dynamic -threaded -with-rtsopts "-N" -o $@ $<

solve: solve.c Makefile
	gcc -O3 -march=native -DDEBUG_STEP=1 -DCPUEXT=1 -o $@ $< # -g -pg

solve-acc: solve.c Makefile
	gcc -O3 -march=native -fopenacc -o $@ $< # -g -pg

solve-cuda: solve.cu Makefile
	nvcc -ccbin g++-5 -arch compute_20 -code sm_21 -O3 -DDEBUG_STEP=1 -o $@ $<
