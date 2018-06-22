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


/* Field type and predefined values
 * unsigned int is enough for 5x5x5
 */

typedef unsigned int field;

typedef struct params {

  int dimension;
  int groupsize;
  int boardsize;
  int allbits;
  char *symbols;
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

void setparams(params_t *p, int dimension, char *syms);

int load(sudoku_t *game, params_t *pars, int pn, char *path);
void unload(sudoku_t *game);

void show(sudoku_t *game);
void showmeta(sudoku_t *game, int s);
void showmeta2(sudoku_t *game);

int solve(sudoku_t *game);
int solve2(sudoku_t *game, int ms);

char *statusmsg(int s);


static int solve1(params_t *p, field *board);

#pragma acc routine seq
static int solvegroup(params_t *p, field *group);

static void showboard(params_t *p, field *board);
static int sym_to_field(params_t *p, char c, field *b);
static char field_to_sym(params_t *p, field b);

static int status(params_t *p, field *board);
static int verify(params_t *p, field *board);

#pragma acc routine seq
static field combination(struct params *p, field *group, field c);

#ifdef CPUEXT

#include <x86intrin.h>

#define nbits _mm_popcnt_u32
#define fstbit __bsfd
//#define clrbit(b) (c = __blsr_u32(b))

#define clrbit(b) (b &= b - 1)

#else

#pragma acc routine seq
static inline int nbits(field b);

#pragma acc routine seq
static inline int fstbit(field b);

#define clrbit(b) (b &= b - 1)

#endif



/* Main functon
 *
 * Load boards from paths gives on the command-line,
 * solve them and print back to stdout.
 */

int main(int argc, char *argv[]) {

  int i, n, s;
  int ms = 10, pn = 5;
  params_t pars[pn];
  sudoku_t game;

  setparams(&pars[0], 5, "abcdefghijklmnopqrstuvwxy");
  setparams(&pars[1], 4, "0123456789abcdef");
  setparams(&pars[2], 4, "123456789abcdefg");
  setparams(&pars[3], 3, "123456789");
  setparams(&pars[4], 2, "1234");

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


void setparams(params_t *p, int dimension, char *syms) {

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
    
    if ((game->board = realloc(game->board, pars[p].boardsize * sizeof(field))) == NULL) {
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

#ifdef COMB_BUF
  game->pars->buf = malloc((game->pars->allbits + 1) * sizeof(field));

  if (game->pars->buf == NULL) {
    fprintf(stderr, "Failed to allocate buffer: %m\n");
    exit(1);
  }
#endif

  return 1;

}

void unload(sudoku_t *game) {
  free(game->board);
#ifdef COMB_BUF
  free(game->pars->buf);
#endif
}


/* Print a messages stating if a board is fully solved,
 * undecided or failed. */

void check(field *board, char *path, int steps) {

  char *st;
  int i, s;


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

  int i, s;
  
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

char *statusmsg(int s) {

  if (s < 0)
    return "failed";
  
  if (s == 0)
    return "undefined";
  
  return "solved";

}


/* Symbol parsing and printing */

static int sym_to_field(params_t *p, char c, field *b) {

  field i;
  char *s;
  
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

  int i, n;

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
  int i, s, n;

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

  if ((board_ = malloc(p->boardsize * sizeof(field))) == NULL) {
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

static int solve1(params_t *p, field *board) {

  int i, j, m = 0;
  field cols[p->boardsize];
  field blocks[p->boardsize];

  
  /* Extract cols and blocks */
  
  for (i = 0; i < p->groupsize; i++) {
    
    for (j = 0; j < p->groupsize; j++) {
      
      cols[i * p->groupsize + j] = board[j * p->groupsize + i];
      
      blocks[i * p->groupsize + j] = board[  (i / p->dimension) * p->groupsize * p->dimension
					   + (i % p->dimension) * p->dimension
					   + (j / p->dimension) * p->groupsize
					   + (j % p->dimension)];
    }
    
  }

  
  /* Solve rows, cols, blocks */
#pragma acc parallel loop reduction(+:m) copy(board[p->boardsize]) copy(cols) copy(blocks)
  for (i = 0; i < p->groupsize; i++) {
    m += solvegroup(p, &cols[(i * p->groupsize)]);
    m += solvegroup(p, &board[i * p->groupsize]);
    m += solvegroup(p, &blocks[i * p->groupsize]);
  }


  /* Integrate results */
  
  for (i = 0; i < p->groupsize; i++) {

    for (j = 0; j < p->groupsize; j++) {
      
      board[i * p->groupsize + j] &= cols[j * p->groupsize + i];
      
      board[i * p->groupsize + j] &= blocks[  (i / p->dimension) * p->groupsize * p->dimension
					   + (i % p->dimension) * p->dimension
					   + (j / p->dimension) * p->groupsize
					   + (j % p->dimension)];

    }
    
  }

#ifdef DEBUG_STEP
  printf("\nStep (%d changes)\n\n", m);
  showboard(p, board);
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

static int solvegroup(params_t *p, field *group) {

  int i, m = 0;
  field b, c, d;
  field def[p->groupsize];
  
#ifdef COMB_BUF

  /* Populate combination buffer */
  
  for (p->buf[0] = i = 0; i < p->groupsize; i++) {

    memcpy(&p->buf[1 << i], p->buf, (1 << i) * sizeof(field));

    for (c = (1 << i); c < (1 << (i+1)); c++)
      p->buf[c] |= group[i];

  }

#endif
  

  /* Calculate defined combinations */
  
  //memset(def, 0, sizeof(def));
  for (i = 0; i < p->groupsize; i++)
    def[i] = 0;
  
  for (c = 1; c < p->allbits; c++) {

#ifdef COMB_BUF
    b = p->buf[c];
#else
    b = combination(p, group, c);
#endif

    if (nbits(c) < nbits(b))
      continue;

    d = c ^ p->allbits;
    while (d) {
      def[fstbit(d)] |= b;
      clrbit(d);
    }

  }


  /* Resolve group */
  
  for (i = 0; i < p->groupsize; i++) {

    b = group[i];
    group[i] &= p->allbits ^ def[i];

    if (group[i] != b)
      m++;

  }

  return m;

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


static field combination(params_t *p, field *group, field c) {

  int j;
  field a = 0;

  while (c) {
    a |= group[fstbit(c)];
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

  int i, n;

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
