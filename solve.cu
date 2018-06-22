/* -*- mode: c -*- */
/*
 * Sudoku solver
 *
 * Author: Maarten Deprez <deprez.maarten@gmail.com>
 */

#define BRUTEFORCE 1 /* go on guessing if deterministic algorithm
			yields an incompletely defined solution */
/*#define COMB_BUF 1*//* use combination buffer; faster, but uses
		      * a block of memory (128MB for 5x5) per
		      * group resolver; not possible yet with
		      * parallelization */
/*#define CPUEXT 1*/ /* use CPU extensions (doesn't work for GPU) */
/*#define DEBUG_STEP 1*/


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <cuda.h>
#include <device_functions.h>


/* Field type and predefined values
 * unsigned int is enough for 5x5x5
 */

typedef unsigned int field;

typedef struct params {

  int dimension;
  int groupsize;
  int boardsize;
  int allbits;
  const char *symbols;
#if 0
  field *groups;
  int *ms;
#endif
#ifdef COMB_BUF
  field *buf;
#endif
} params_t;

typedef struct sudoku {

  struct params *pars;
  field *board;
  char *path;
  int steps;
  int tries;
  int allsteps;
  int alltries;
  int solutions;
} sudoku_t;


/* Function prototypes */

void setparams(params_t *p, int dimension, const char *syms);

int load(sudoku_t *game, params_t *pars, int pn, char *path);
void unload(sudoku_t *game);

void show(sudoku_t *game);
void showmeta(sudoku_t *game, int s);
void showmeta2(sudoku_t *game);

int solve(sudoku_t *game);
int solve2(sudoku_t *game, int ms);

const char *statusmsg(int s);


static int solve1(params_t *p, field *board);

static __global__ void solvegroup(int boardsize, int groupsize, int allbits);

static void showboard(params_t *p, field *board);
static int sym_to_field(params_t *p, char c, field *b);
static char field_to_sym(params_t *p, field b);

static int status(params_t *p, field *board);
static int verify(params_t *p, field *board);

static __device__ field combination(const field *group, field c);

#ifdef CPUEXT

#include <x86intrin.h>

#define nbits _mm_popcnt_u32
#define fstbit __bsfd
//#define clrbit(b) (c = __blsr_u32(b))

#define clrbit(b) (b &= b - 1)

#else

static inline int nbits(field b);
static inline int fstbit(field b);

#define clrbit(b) (b &= b - 1)

#endif



/* Main functon
 *
 * Load boards from paths gives on the command-line,
 * solve them and print back to stdout.
 */

int main(int argc, char *argv[]) {

  int i, s;
  int ms = 10, pn = 5;
  params_t pars[pn];
  sudoku_t game;

  setparams(&pars[0], 5, "abcdefghijklmnopqrstuvwxy");
  setparams(&pars[1], 4, "0123456789abcdef");
  setparams(&pars[2], 4, "123456789abcdefg");
  setparams(&pars[3], 3, "123456789");
  setparams(&pars[4], 2, "1234");

  cudaSetDeviceFlags(cudaDeviceScheduleYield);

  for (i = 1; i < argc; i++) {

    if (i > 1) {
      putchar('\n');
      putchar('\n');
    }

    if (!load(&game, pars, pn, argv[i])) {
      fprintf(stderr, "Invalid input: %s\n", argv[i]);
      return 1;
    }

#ifndef BRUTEFORCE /* Deterministic solution */

    s = solve(&game);
    showmeta(&game, s);
    show(&game);
    
#else /* Deterministic + brute-force --> all solutions */

    /* Solve2 will print for us... */

    s = solve2(&game, ms);

    if (game.solutions >= ms) {
      fprintf(stderr, "\nBailing out after %d solutions.\n", ms);

    } else if (s < 0) {
      showmeta(&game, s);
      show(&game);
    }

#endif

    showmeta2(&game);
    
    unload(&game);
    
  }
  
  return 0;

}


void setparams(params_t *p, int dimension, const char *syms) {

  p->dimension = dimension;
  p->groupsize = p->dimension * p->dimension;
  p->boardsize = p->groupsize * p->groupsize;
  p->allbits = (1 << p->groupsize) - 1;
  p->symbols = syms;  
}


/* Loading and printing
 *
 * Errors cause the program to abort here,
 * so functions calling us do not need to do
 * error-checking. */

/* Load a board from file.
 * 
 * This simply reads groupsize optionally whitespace
 * -separated symbols from the given file. The input
 * may be formatted, but this is not a requirement.
 */

int load(sudoku_t *game, params_t *pars, int pn, char *path) {

  int i, p, fd, len;
  char buf[1024], *s;

  /* Open file */
  
  if ((fd = open(path, O_RDONLY)) == -1) {
    fprintf(stderr, "Could not open file %s: %m\n", path);
    exit(1);
  }

  /* Read data */
  
  if ((len = read(fd, buf, sizeof(buf))) == -1) {
    fprintf(stderr, "Could not read from file: %m\n");
    exit(1);
  }

  close(fd);

  game->board = NULL;


  /* Try params in order */

  for (p = 0; p < pn; p++) {
    
    /* Init parser */
  
    buf[len] = 0; /* null is not space and not a symbol, */
    s = buf;      /* so passing over it will always fail */

    /* Allocate board */
    
    if ((game->board = (field *) realloc(game->board, pars[p].boardsize * sizeof(field))) == NULL) {
      fprintf(stderr, "Could not allocate space for board: %m\n");
      exit(1);
    }


    /* Fill the board */
  
    for (i = 0; i < pars[p].boardsize; i++) {

      /* Skip spaces */
      while (*s == ' ' || *s == '\t' || *s == '\n')
	s++;

      /* Get one symbol */
      if (!sym_to_field(&pars[p], *s, &(game->board[i])))
	break;

      s++;

    }

    /* Check if all fields could be read */
    if (i == pars[p].boardsize) {
      break;
    }
    
  }

  /* Fail if we ran out of possible pars */
  if (p == pn) {
    free(game->board);
    return 0;
  }

  
  /* Store pars in game */

  game->pars = &pars[p];
  game->steps = 0;
  game->tries = 0;
  game->allsteps = 0;
  game->alltries = 0;
  game->solutions = 0;
  game->path = path;
#if 0
  game->pars->groups = NULL;
  game->pars->ms = NULL;

  if (cudaMalloc((void **)&(game->pars->groups), sizeof(field) * game->pars->boardsize * 3) != cudaSuccess ||
      cudaMalloc((void **)&(game->pars->ms), sizeof(int) * game->pars->groupsize * 3) != cudaSuccess) {
    fprintf(stderr, "Failed to allocate CUDA groups buffer\n");
    exit(1);
  }
#endif
#ifdef COMB_BUF
  game->pars->buf = NULL;

  if (cudaMalloc((void **)&game->pars->buf, (game->pars->allbits + 1) * sizeof(field) * game->pars->groupsize * 3) != cudaSuccess) {
    fprintf(stderr, "Failed to allocate CUDA combinations buffer\n");
    exit(1);
  }
#endif

  return 1;

}

void unload(sudoku_t *game) {
  free(game->board);
#ifdef COMB_BUF
  cudaFree(game->pars->buf);
#endif
}


/* Print solution information to standard output */
void showmeta(sudoku_t *game, int s) {

  if (s > 0) {

    printf("\nSudoku %s, solution %d: %s in %d steps, %d tries:\n\n",
	   game->path, game->solutions, statusmsg(s),
	   game->steps, game->tries);

  } else {

    printf("\nSudoku %s: %s after %d steps, %d tries:\n\n",
	   game->path, statusmsg(s),
	   game->steps, game->tries);
  }

}


/* Print total work information to standard output */
void showmeta2(sudoku_t *game) {
  printf("\nSudoku %s: found %d solution(s) in %d steps, %d tries\n",
	 game->path, game->solutions,
	 game->allsteps, game->alltries);
}

/* Print board to standard output.
 * 
 * This prints the fields as symbols, interspersed with
 * whitespace for formatting.
 */
void show(sudoku_t *game) {
  showboard(game->pars, game->board);
}

static void showboard(params_t *p, field *board) {

  int i;
  
  for (i = 0; i < p->boardsize; i++) {

    if (i == 0) {
    } else if (i % (p->groupsize * p->dimension) == 0) {
      putchar('\n');
      putchar('\n');
    } else if (i % p->groupsize == 0) {
      putchar('\n');
    } else if (i % p->dimension == 0) {
      putchar(' ');
      putchar(' ');
    } else {
      putchar(' ');
    }

    putchar(field_to_sym(p, board[i]));
    
  }

  putchar('\n');

}


/* Status message */

const char *statusmsg(int s) {

  if (s < 0)
    return "failed";
  
  if (s == 0)
    return "undefined";
  
  return "solved";

}


/* Symbol parsing and printing */

static int sym_to_field(params_t *p, char c, field *b) {

  field i;
  const char *s;
  
  if (c == '_' || c == '.') {
    *b = p->allbits;
    return 1;
  }
  
  if (c == '*') {
    *b = 0;
    return 1;
  }

  for (i = 1, s = p->symbols; *s; s++, i <<= 1)
    if (c == *s)
      break;

  if (*s) {
    *b = i;
    return 1;
  }

  return 0;

}

static char field_to_sym(params_t *p, field b) {

  if (b == 0)
    return '*';
  if (b == p->allbits)
    return '_';
  if (nbits(b) > 1)
    return '.';

  return p->symbols[fstbit(b)];

}


/* Resolution */

int solve(sudoku_t *game) {

  params_t *p = game->pars;
  field *board = game->board;
  int s;
  
  while (status(p, board) == 0 && solve1(p, board)) {
    game->steps++;
    game->allsteps++;
  }
  
  s = verify(p, board);

  if (s > 0)
    game->solutions++;

  return s;

}


/* Brute-force when solution got stuck
 * and print all possible solutions to
 * standard output.
 */

int solve2(sudoku_t *game, int ms) {

  params_t *p = game->pars;
  field *board = game->board;

  field b;
  int i, s;

  field *board_;
  int steps_, tries_;


  /* Try to solve deterministically */

  while (status(p, board) == 0 && solve1(p, board)) {
    game->steps++;
    game->allsteps++;
  }

  if ((s = verify(p, board)) != 0) {

    if (s > 0) {
      game->solutions++;
      showmeta(game, s);
      show(game);
    }

    return s;

  }


  /* Copy the original */

  if ((board_ = (field *) malloc(p->boardsize * sizeof(field))) == NULL) {
    fprintf(stderr, "Could not allocate memory for board: %m\n");
    exit(1);
  }

  memcpy(board_, board, p->boardsize * sizeof(field));
  steps_ = game->steps;
  tries_ = game->tries;


  /* Select first undetermined field */
  
  for (i = 0; i < p->boardsize; i++)
    if (nbits(board[i]) > 1)
      break;

  
  /* Try every possibility */

  b = board[i];
  while (b) {

    board[i] = 1 << fstbit(b);
    game->tries++;
    game->alltries++;
    clrbit(b);
    
    /* Recurse */

    solve2(game, ms);

    if (game->solutions >= ms)
      break;

    /* Revert to original */

    memcpy(board, board_, p->boardsize * sizeof(field));
    game->steps = steps_;
    game->tries = tries_;

  }


  /* Clean up */

  free(board_);
  return game->solutions ? 1 : -1;

}


/* One resolution step
 *
 * Solves rows, cols and blocks.
 * The return value indicates whether a change
 * was made (and another step might be helpful).
 * 
 * This function just does the copying around. The
 * actual resolution is done in solvegroup.
 */

static __constant__ const field groupsin[32*25*3] = {0};
static __device__ field groupsout[32*25*3];
static __device__ int msout[25 * 3];

static int solve1(params_t *p, field *board) {

  int i, j, m = 0;

  int ms[p->groupsize * 3];
  field groups[32 * p->groupsize * 3];

  field *rows   = &groups[0 * 32 * p->groupsize];
  field *cols   = &groups[1 * 32 * p->groupsize];
  field *blocks = &groups[2 * 32 * p->groupsize];

  cudaError_t err;


  /* Extract cols and blocks */
  
  for (i = 0; i < p->groupsize; i++) {
    
    for (j = 0; j < p->groupsize; j++) {

      rows[i * 32 + j] = board[i * p->groupsize + j];
      cols[i * 32 + j] = board[j * p->groupsize + i];
      
      blocks[i * 32 + j] = board[  (i / p->dimension) * p->groupsize * p->dimension
				 + (i % p->dimension) * p->dimension
				 + (j / p->dimension) * p->groupsize
				 + (j % p->dimension)];
    }
    
  }

  
  /* Solve rows, cols, blocks */

  if ((err = cudaMemcpyToSymbol(groupsin, groups, sizeof(field) * p->groupsize * 32 * 3)) != cudaSuccess) {
    fprintf(stderr, "Host to device copy failed: %s!\n", cudaGetErrorString(err));
    exit(1);
  }

  solvegroup<<<3,p->groupsize>>>(p->boardsize, p->groupsize, p->allbits);

  if ((err = cudaGetLastError()) != cudaSuccess) {
    fprintf(stderr, "Kernel execution failed: %s!\n", cudaGetErrorString(err));
    exit(1);    
  }

  if ((err = cudaMemcpyFromSymbol(groups, groupsout, sizeof(field) * p->groupsize * 32 * 3)) != cudaSuccess ||
      (err = cudaMemcpyFromSymbol(ms,     msout,     sizeof(int) * p->groupsize * 3)) != cudaSuccess) {
    fprintf(stderr, "Device to host copy failed: %s!\n", cudaGetErrorString(err));
    exit(1);
  }


  /* Integrate results */
  
  for (i = 0; i < p->groupsize; i++) {

    for (j = 0; j < p->groupsize; j++) {

      board[i * p->groupsize + j] =

	rows[i * 32 + j] &
	cols[j * 32 + i] &
        blocks[  (i / p->dimension) * 32 * p->dimension
	       + (i % p->dimension) * p->dimension
	       + (j / p->dimension) * 32
	       + (j % p->dimension)];

    }
    
  }


  for (i = 0; i < p->groupsize * 3; i++)
    m += ms[i];

  
#ifdef DEBUG_STEP
  printf("\nStep (%d changes)\n\n", m);
  showboard(p, board);
  exit(0);
#endif

  return m;

}


/* Solve one group (row, column, block).
 *
 * For every field in the group, all values defined with
 * certainty in any of the combinations of the other
 * elements is set to zero.
 *
 * To do this, we loop over all possible combinations, denoted
 * by the bits in a counter from 1 to allbits, and save bitfields 
 * to a buffer marking the values that must be present in every
 * combination. This bitfield is found by or'ing all fields contained
 * in the combination, giving a list of all possible values in the
 * combination, and write this value if there are at most as many
 * possibilities as there are elements in the combination, or else zero.
 *
 * The values of the buffer can then be or'ed together for every index
 * for which the n'th bit is zero to get a bitfield of the defined values
 * in all combinations that do not include field n.
 */


static __global__ void solvegroup(int boardsize, int groupsize, int allbits) {

  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  const field *group = &groupsin[idx * 32];
  field *groupout = &groupsout[idx * 32];

  __shared__ field f[25];
  __shared__ field g[25];
  
  int m = 0;
  field b, c;


  g[0] = group[0];
  g[1] = group[1];
  g[2] = group[2];
  g[3] = group[3];
  g[4] = group[4];
  g[5] = group[5];
  g[6] = group[6];
  g[7] = group[7];
  g[8] = group[8];
  g[9] = group[9];
  g[10] = group[10];
  g[11] = group[11];
  g[12] = group[12];
  g[13] = group[13];
  g[14] = group[14];
  g[15] = group[15];
  g[16] = group[16];
  g[17] = group[17];
  g[18] = group[18];
  g[19] = group[19];
  g[20] = group[20];
  g[21] = group[21];
  g[22] = group[22];
  g[23] = group[23];
  g[24] = group[24];


  /* Calculate defined combinations */
  
  f[0] = 0;
  f[1] = 0;
  f[2] = 0;
  f[3] = 0;
  f[4] = 0;
  f[5] = 0;
  f[6] = 0;
  f[7] = 0;
  f[8] = 0;
  f[9] = 0;
  f[10] = 0;
  f[11] = 0;
  f[12] = 0;
  f[13] = 0;
  f[14] = 0;
  f[15] = 0;
  f[16] = 0;
  f[17] = 0;
  f[18] = 0;
  f[19] = 0;
  f[20] = 0;
  f[21] = 0;
  f[22] = 0;
  f[23] = 0;
  f[24] = 0;

  for (c = 1; c < allbits; c++) {

    b =
      ((c & (1 << 0)) ? g[0] : 0) |
      ((c & (1 << 1)) ? g[1] : 0) |
      ((c & (1 << 2)) ? g[2] : 0) |
      ((c & (1 << 3)) ? g[3] : 0) |
      ((c & (1 << 4)) ? g[4] : 0) |
      ((c & (1 << 5)) ? g[5] : 0) |
      ((c & (1 << 6)) ? g[6] : 0) |
      ((c & (1 << 7)) ? g[7] : 0) |
      ((c & (1 << 8)) ? g[8] : 0) |
      ((c & (1 << 9)) ? g[9] : 0) |
      ((c & (1 << 10)) ? g[10] : 0) |
      ((c & (1 << 11)) ? g[11] : 0) |
      ((c & (1 << 12)) ? g[12] : 0) |
      ((c & (1 << 13)) ? g[13] : 0) |
      ((c & (1 << 14)) ? g[14] : 0) |
      ((c & (1 << 15)) ? g[15] : 0) |
      ((c & (1 << 16)) ? g[16] : 0) |
      ((c & (1 << 17)) ? g[17] : 0) |
      ((c & (1 << 18)) ? g[18] : 0) |
      ((c & (1 << 19)) ? g[19] : 0) |
      ((c & (1 << 20)) ? g[20] : 0) |
      ((c & (1 << 21)) ? g[21] : 0) |
      ((c & (1 << 22)) ? g[22] : 0) |
      ((c & (1 << 23)) ? g[23] : 0) |
      ((c & (1 << 24)) ? g[24] : 0);

    if (__popc(c) < __popc(b))
      continue;

    f[0] |= (c & (1 << 0)) ? 0 : b;
    f[1] |= (c & (1 << 1)) ? 0 : b;
    f[2] |= (c & (1 << 2)) ? 0 : b;
    f[3] |= (c & (1 << 3)) ? 0 : b;
    f[4] |= (c & (1 << 4)) ? 0 : b;
    f[5] |= (c & (1 << 5)) ? 0 : b;
    f[6] |= (c & (1 << 6)) ? 0 : b;
    f[7] |= (c & (1 << 7)) ? 0 : b;
    f[8] |= (c & (1 << 8)) ? 0 : b;
    f[9] |= (c & (1 << 9)) ? 0 : b;
    f[10] |= (c & (1 << 10)) ? 0 : b;
    f[11] |= (c & (1 << 11)) ? 0 : b;
    f[12] |= (c & (1 << 12)) ? 0 : b;
    f[13] |= (c & (1 << 13)) ? 0 : b;
    f[14] |= (c & (1 << 14)) ? 0 : b;
    f[15] |= (c & (1 << 15)) ? 0 : b;
    f[16] |= (c & (1 << 16)) ? 0 : b;
    f[17] |= (c & (1 << 17)) ? 0 : b;
    f[18] |= (c & (1 << 18)) ? 0 : b;
    f[19] |= (c & (1 << 19)) ? 0 : b;
    f[20] |= (c & (1 << 20)) ? 0 : b;
    f[21] |= (c & (1 << 21)) ? 0 : b;
    f[22] |= (c & (1 << 22)) ? 0 : b;
    f[23] |= (c & (1 << 23)) ? 0 : b;
    f[24] |= (c & (1 << 24)) ? 0 : b;

  }

  
  /* Resolve group */
  
  groupout[0] = g[0] & (allbits ^ f[0]);
  groupout[1] = g[1] & (allbits ^ f[1]);
  groupout[2] = g[2] & (allbits ^ f[2]);
  groupout[3] = g[3] & (allbits ^ f[3]);
  groupout[4] = g[4] & (allbits ^ f[4]);
  groupout[5] = g[5] & (allbits ^ f[5]);
  groupout[6] = g[6] & (allbits ^ f[6]);
  groupout[7] = g[7] & (allbits ^ f[7]);
  groupout[8] = g[8] & (allbits ^ f[8]);
  groupout[9] = g[9] & (allbits ^ f[9]);
  groupout[10] = g[10] & (allbits ^ f[10]);
  groupout[11] = g[11] & (allbits ^ f[11]);
  groupout[12] = g[12] & (allbits ^ f[12]);
  groupout[13] = g[13] & (allbits ^ f[13]);
  groupout[14] = g[14] & (allbits ^ f[14]);
  groupout[15] = g[15] & (allbits ^ f[15]);
  groupout[16] = g[16] & (allbits ^ f[16]);
  groupout[17] = g[17] & (allbits ^ f[17]);
  groupout[18] = g[18] & (allbits ^ f[18]);
  groupout[19] = g[19] & (allbits ^ f[19]);
  groupout[20] = g[20] & (allbits ^ f[20]);
  groupout[21] = g[21] & (allbits ^ f[21]);
  groupout[22] = g[22] & (allbits ^ f[22]);
  groupout[23] = g[23] & (allbits ^ f[23]);
  groupout[24] = g[24] & (allbits ^ f[24]);

  m =
    (groupout[0] == g[0] ? 0 : 1) + 
    (groupout[1] == g[1] ? 0 : 1) + 
    (groupout[2] == g[2] ? 0 : 1) + 
    (groupout[3] == g[3] ? 0 : 1) +
    (groupout[4] == g[4] ? 0 : 1) + 
    (groupout[5] == g[5] ? 0 : 1) + 
    (groupout[6] == g[6] ? 0 : 1) + 
    (groupout[7] == g[7] ? 0 : 1) + 
    (groupout[8] == g[8] ? 0 : 1) + 
    (groupout[9] == g[9] ? 0 : 1) +
    (groupout[10] == g[10] ? 0 : 1) + 
    (groupout[11] == g[11] ? 0 : 1) + 
    (groupout[12] == g[12] ? 0 : 1) + 
    (groupout[13] == g[13] ? 0 : 1) + 
    (groupout[14] == g[14] ? 0 : 1) + 
    (groupout[15] == g[15] ? 0 : 1) +
    (groupout[16] == g[16] ? 0 : 1) + 
    (groupout[17] == g[17] ? 0 : 1) + 
    (groupout[18] == g[18] ? 0 : 1) + 
    (groupout[19] == g[19] ? 0 : 1) + 
    (groupout[20] == g[20] ? 0 : 1) + 
    (groupout[21] == g[21] ? 0 : 1) + 
    (groupout[22] == g[22] ? 0 : 1) + 
    (groupout[23] == g[23] ? 0 : 1) + 
    (groupout[24] == g[24] ? 0 : 1);

  msout[idx] = m;

}

#ifndef CPUEXT

static inline int nbits(field b) {

  int n = 0;
  
  while (b) {
    n += b & 1;
    b >>= 1;
  }

  return n;
  
}

static inline int fstbit(field b) {

  int i = 0;

  if (b == 0)
    return -1;
  
  while ((b & 1) == 0) {
    i++;
    b >>= 1;
  }
  
  return i;

}

#endif


static __device__ inline field combination(const field *group, field c) {

  field a = 0;

  while (c) {
    a |= group[__ffs(c) - 1];
    clrbit(c);
  }
  
  return a;

}


/* Verify status of board:
 * if one value is 0, status is failed (-1)
 * if one value has more than 1 bit set, status is undefined (0)
 * if all values have exactly 1 bit set, status is solved (1)
 */

static int status(params_t *p, field *board) {

  int i;

  /* Check for failure */
  for (i = 0; i < p->boardsize; i++)
    if (board[i] == 0)
      return -1;

  /* Check for undef */
  for (i = 0; i < p->boardsize; i++)
    if (nbits(board[i]) > 1)
      return 0;

  return 1;

}


/* Verify correctness */

static int verify(params_t *p, field *board) {

  field a;
  int i, j, s;

  
  s = status(p, board);

  if (s < 0)
    return s;


  /* Check rows */

  for (i = 0; i < p->groupsize; i++) {
    
    for (j = 0, a = 0; j < p->groupsize; j++)
      a |= board[i * p->groupsize + j];
    
    if (a != p->allbits)
      return -1;
  }

  
  /* Check columns */

  for (i = 0; i < p->groupsize; i++) {
    
    for (j = 0, a = 0; j < p->groupsize; j++)
      a |= board[j * p->groupsize + i];
    
    if (a != p->allbits)
      return -1;
  }


  /* Check blocks */

  for (i = 0; i < p->groupsize; i++) {
    
    for (j = 0, a = 0; j < p->groupsize; j++)
      a |= board[  (i / p->dimension) * p->groupsize * p->dimension
		 + (i % p->dimension) * p->dimension
		 + (j / p->dimension) * p->groupsize
		 + (j % p->dimension)];
    
    if (a != p->allbits)
      return -1;
  }

  return s;
  
}
