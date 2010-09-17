/*
  dense univariate gcd reference implementation
  for the PASCO 2010 programming contest 
  Roman Pearce, CECM/SFU, June 2010

  you can use the input and output functions
  in your program to read and write files
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gmp.h"

/* print for debugging purposes */
#if 0
#define PRINT gmp_printf
#else
#define PRINT(...)
#endif


/* read file into dense array */
mpz_t * read_poly(FILE *file, int *size) {
	mpz_t c, *poly = NULL;
	int L, S, e, i;

	mpz_init(c);	/* get size and first term */
	gmp_fscanf(file, "%d\n %d %Zd", &L, &e, &c);
	PRINT("read terms=%d size=%d\n", L, e+1);
	PRINT("p[%d] = %Zd\n", e, c);

	S = e+1;	/* allocate dense array */
	poly = malloc(S*sizeof(mpz_t));
	memset((void *)poly, 0, S*sizeof(mpz_t));

	mpz_set(poly[e], c);
	for (i=1; i < L; i++) {
		gmp_fscanf(file, "%d %Zd\n", &e, &c);
		PRINT("p[%d] = %Zd\n", e, c);
		mpz_set(poly[e], c);
	}

	*size = S;
	mpz_clear(c);
	return poly;
}


/* write dense array to file */
void write_poly(FILE *file, mpz_t *poly, int size) {
	int L=0, d, i;

	/* count non-zero terms */
	for (i=0; i < size; i++) {
		if (mpz_sgn(poly[i])) L++;
	}
	PRINT("write terms=%d size=%d\n", L, size);

	/* write terms to file */
	gmp_fprintf(file, "%d\n", L);
	for (i=size-1; i >= 0; i--) {
		if (mpz_sgn(poly[i])) {
			PRINT("p[%d] = %Zd\n", i, poly[i]);
			gmp_fprintf(file, "%d %Zd\n", i, poly[i]);
		}
	}
}	


/* copy coefficient words to integer */
void kronecker_subs(mpz_t A, mpz_t *a, int sa, int w) {
	int i, j, s;
	mpz_t X;

	/* positive coefficients in A */
	/* negative coefficients in X */
	mpz_init2(A, sa*w*sizeof(mp_limb_t)*8);
	mpz_init2(X, sa*w*sizeof(mp_limb_t)*8);
	memset((void *)A->_mp_d, 0, sa*w*sizeof(mp_limb_t));
	memset((void *)X->_mp_d, 0, sa*w*sizeof(mp_limb_t));

	/* copy coefficients into A or X */
	for (i=0; i < sa; i++) {
		s = mpz_sgn(a[i]);
		if (s > 0) for (j=0; j < w; j++) A->_mp_d[i*w+j] = mpz_getlimbn(a[i],j);
		if (s < 0) for (j=0; j < w; j++) X->_mp_d[i*w+j] = mpz_getlimbn(a[i],j);
	}

	/* normalize A and X */
	for (s=sa*w; A->_mp_d[--s]==(mp_limb_t)0 && s >= 0;);
	A->_mp_size = ++s;

	for (s=sa*w; X->_mp_d[--s]==(mp_limb_t)0 && s >= 0;);
	X->_mp_size = ++s;

	/* subtract to get result */
	mpz_sub(A, A, X);
	PRINT("=> %Zd\n", A);
	mpz_clear(X);
}


/* copy integer words into coefficients */
mpz_t * kronecker_undo(mpz_t C, int w, int *sc) {
	int i, j, S, s, carry;
	mpz_t z, t, *c;

	mpz_init2(z, w*sizeof(mp_limb_t)*8);
	mpz_init_set_ui(t, 1);
	mpz_mul_2exp(t, t, w*sizeof(mp_limb_t)*8);

	S = (C->_mp_size + w-1)/w;
	c = malloc(S*sizeof(mpz_t));
	memset((void *)c, 0, S*sizeof(mpz_t));

	carry = 0;
	for (i=0; i < S; i++) {
		for (j=0; j < w; j++) {
			z->_mp_d[j] = mpz_getlimbn(C, i*w+j);
		}
		/* normalize */
		for (s=w; z->_mp_d[--s]==(mp_limb_t)0 && s >= 0;);
		z->_mp_size = ++s;

		/* negative coefficients set carry */
		/* to be added to next coefficient */
		mpz_init(c[i]);
		if (s < w) {
			mpz_add_ui(c[i], z, carry);
			carry = 0;
		}
		else {
			mpz_add_ui(z, z, carry);
			mpz_sub(c[i], z, t);
			carry = 1;
		}
	}
	mpz_clear(z);
	mpz_clear(t);

	*sc = S;
	return c;
}


/* gcd by kronecker substitution */
mpz_t * kronecker_gcd(mpz_t *a, int sa, mpz_t *b, int sb, int *sc) {
	int i, t, w=0;
	mpz_t A, B, C, *c;

	/* size of max coefficient */
	for (i=0; i < sa; i++) {
		t = mpz_size(a[i]);
		w = (t > w) ? t : w;
	}
	for (i=0; i < sb; i++) {
		t = mpz_size(b[i]);
		w = (t > w) ? t : w;
	}
	w++;	/* add an extra word */
	/* this wastes a lot of bits */
	/* we store x < 0 as 2^w + x */
	/* then use the highest word */
	/* to check for negative int */
	PRINT("cofs = %d words\n", w);

	kronecker_subs(A, a, sa, w);
	kronecker_subs(B, b, sb, w);

	mpz_init(C);
	mpz_gcd(C,A,B);
	PRINT("=> %Zd\n", C);

	c = kronecker_undo(C, w, sc);
	mpz_clear(A);
	mpz_clear(B);
	mpz_clear(C);

	return c;
}


int main(int argc, char *argv[]) {
	FILE  *A, *B, *C;	/* file stream */
	mpz_t *a, *b, *c;	/* polynomials */
	int   sa, sb, sc;	/* nb of terms */
	int   i;

	if (argc != 4) {
		printf("input: polyA polyB gcdfile\n");
		return 1;
	}

	A = fopen(argv[1], "r");
	a = read_poly(A, &sa);
	fclose(A);

	B = fopen(argv[2], "r");
	b = read_poly(B, &sb);
	fclose(B);

	/* make leading coeff positive */
	if (mpz_sgn(a[sa-1]) < 1) for (i=0; i < sa; i++) mpz_neg(a[i],a[i]);
	if (mpz_sgn(b[sb-1]) < 1) for (i=0; i < sb; i++) mpz_neg(b[i],b[i]);

	c = kronecker_gcd(a, sa, b, sb, &sc);

	C = fopen(argv[3], "w");
	write_poly(C, c, sc);
	fclose(C);

	for (i=0; i < sa; i++) mpz_clear(a[i]);
	for (i=0; i < sb; i++) mpz_clear(b[i]);
	for (i=0; i < sc; i++) mpz_clear(c[i]);
	free(a);
	free(b);
	free(c);

	return 0;
}



/* maple code to read and write files */
/* and to generate benchmark problems */
#if 0

readpol := proc(file::{name,string}, x::name)
  local A, i;
   A := readdata(file, integer, 2);
   return `+`(seq(x^i[1]*i[2], i=A[2..-1]));
end proc:


writepol := proc(file::{name,string}, poly)
  local fd, x, C, M, i;
   x := indets(poly);
   if not (nops(x)=1 and type(poly,polynom(integer,x))) then error "invalid poly"; end if;
   C, M := [coeffs(poly,x,'M')], [M];
   C := [seq([degree(M[i]),C[i]], i=1..nops(C))];
   C := sort(C, (a,b)->evalb(a[1] > b[1]));

   fd := fopen(file,WRITE);
   fprintf(fd, "%d\n", nops(C));
   for i in C do fprintf(fd, "%d %d\n", op(i)); end do;
   fclose(fd);
   return NULL;
end proc:


makebench := proc(df, dg, bf, bg, fname:=FAIL) 
  local rf, rg, f1, f2, g;
   rf := rand(-2^(bf-bg)..2^(bf-bg)):
   rg := rand(-2^bg..2^bg):  
   f1,f2 := seq(randpoly(x,degree=df,dense,coeffs=rf),i=1..2):
   g := randpoly(x,degree=dg,dense,coeffs=rg):
   if (fname=FAIL) then
     writepol(cat(df,_,bf,a), expand(f1*g)):
     writepol(cat(df,_,bf,b), expand(f2*g)):
   else
     writepol(cat(fname,a), expand(f1*g)):
     writepol(cat(fname,b), expand(f2*g)):
   end if;
end proc:


# input degree, gcd degree, input bits, gcd bits
makebench(100, 10, 20, 10, fun);                    # fun
makebench(1000, 100, 1000, 500, tricky);            # tricky
makebench(10000, 1000, 1000, 500, taxing);          # taxing
makebench(20000, 5000, 10000, 5000, mayhem);        # mayhem

make -s
time ./gcd funa funb func
time ./gcd trickya trickyb trickyc
time ./gcd taxinga taxingb taxingc
time ./gcd mayhema mayhemb mayhemc

#endif
