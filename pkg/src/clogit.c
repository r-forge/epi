#include <R.h>
#include <Rinternals.h>

static int imin(int i, int j)
{
    return i < j ? i : j;
}

/* 
   Efficient calculation of the conditional log likelihood, along with
   the score and information matrix, for a single stratum.

   Input parameters:

   X      T x m matrix of covariate values
   y      T-vector that indicates if an individual is a case (y[t]==1)
            or control (y[t]==0)
   T      Number of individuals in the stratum
   m      Number of covariates
   beta   m-vector of log odds ratio parameters

   Output parameters:

   loglik The conditional log-likelihood (scalar)
   score  The score function (m-vector)
   info   The information matrix (m x m matrix)
   
   The contribution from this stratum will be *added* to the output
   parameters, so they must be correctly initialized before calling
   cloglik.
   
*/

void cloglik(double const *X, int const *y, int T, int m, double const *beta, 
	     double *loglik, double *score, double *info)
{
    double *f, *g, *h, *xt;
    int i,j,k,t;
    int K = 0, Kp;
    int iscase = 1;
    double sign = 1;

    /* Calculate number of cases */
    for (t = 0; t < T; ++t) {
	if (y[t] != 0 && y[t] != 1) {
	    error("Invalid outcome in conditional log likelihood");
	}
	K += y[t];
    }
    if (K==0 || K==T) {
	return; /* Non-informative stratum */
    }

    /* 
       If there are more cases than controls then define cases to be
       those with y[t] == 0, and reverse the sign of the covariate values.
    */
    if (K > T/2) {
	K = T - K;
	iscase = 0;
	sign = -1;
    }

    /* Contribution from cases */

    for (t = 0; t < T; ++t) {
	if (y[t] == iscase) {
	    for (i = 0; i < m; ++i) {
		loglik[0] += sign * X[t + i*T] * beta[i];
		score[i] += sign * X[t + i*T];
	    }
	}
    }
    
    /* Allocate and initialize workspace for recursive calculations */

    Kp = K + 1;
    f = Calloc(Kp, double);
    g = Calloc(m * Kp, double);
    h = Calloc(m * m * Kp, double);
    xt = Calloc(m, double);

    for (k = 0; k < Kp; ++k) {
	f[k] = 0;
	for (i = 0; i < m; ++i) {
	    g[k+Kp*i] = 0;
	    for (j = 0; j < m; ++j) {
		h[k + Kp*(i + m*j)] = 0;
	    }
	}
    }
    f[0] = 1;

    /* 
       Recursively calculate contributions from summing over all
       possible case sets of size K.
    */

    for (t = 0; t < T; ++t) {

	double Ct = 0;
	for (i = 0; i < m; ++i) {
	    xt[i] = sign * X[t + T*i];
	    Ct += beta[i] * xt[i];
	}
	Ct = exp(Ct);

	for (k = imin(K,t+1); k > 0; --k) {

	    for (i = 0; i < m; ++i) {
		double const *gpi = g + Kp*i;
		for (j = 0; j < m; ++j) {
		    double const *gpj = g + Kp*j;
		    double *hp = h + Kp*(i + m*j);
		    hp[k] += Ct * (hp[k-1] + 
				   xt[i] * gpj[k-1] + 
				   xt[j] * gpi[k-1] +
				   xt[i] * xt[j] * f[k-1]);
		}
	    }

	    for (i = 0; i < m; ++i) {
		double *gp = g + Kp*i;
		gp[k] += Ct * (gp[k-1] + xt[i] * f[k-1]);
	    }

	    f[k] += Ct * f[k-1];
	}

    }

    /* Add contributions from this stratum */

    loglik[0] -= log(f[K]);
    for (i = 0; i < m; ++i) {
	double const *gpi = g + Kp*i;
	score[i] -= gpi[K] / f[K];
	for (j = 0; j < m; ++j) {
	    double const *gpj = g + Kp*j;
	    double const *hp = h + Kp*(i + m*j);
	    info[i + m*j] += hp[K]/f[K] - (gpi[K]/f[K]) * (gpj[K]/f[K]);
	}
    }

    Free(f);
    Free(g);
    Free(h);
    Free(xt);
}

/*
 * .Call Interface to do conditional log likelihood calculations
 *
 * Input parameters:
 *
 * X    - list of matrices of covariate values. One element of the list
 *        corresponds to a single stratum
 * Y    - list of vectors of outcomes, corresponding to X,
 * init - vector of initial values for log odds ratio
 *
 * Returns *minus* the conditional log likelihood, with its gradient
 * and hessian as attributes. This is the format required by nlm.
 */

SEXP neg_cloglik(SEXP X, SEXP y, SEXP beta)
{
    int i;
    int n = length(X);
    int m = length(beta);
    int M = m*m;
    double *bp = REAL(beta);
    double loglik, *score, *info;
    SEXP ans, grad, hess, dims;

    if (!isNewList(X)) error("'X' must be a list");
    if (!isNewList(y)) error("'y' must be a list");
    if (length(X) != length(y)) error("length mismatch between X and y");

    /* Output parameters of cloglik must be initialized to zero */
    loglik = 0;
    score = (double *) R_alloc(m, sizeof(double));
    for (i = 0; i < m; ++i) {
	score[i] = 0;
    }
    info = (double *) R_alloc(M, sizeof(double));
    for (i = 0; i < M; ++i) {
	info[i] = 0;
    }
    
    for (i = 0; i < n; ++i) {

	SEXP Xi, yi;
	int T = nrows(VECTOR_ELT(X,i));
        int xcols = ncols(VECTOR_ELT(X,i));
        int ylen  = length(VECTOR_ELT(y,i));

	if (xcols != m) {
	    error("Element %d of X has %d columns; expected %d", i, xcols, m);
	}
	if (ylen != T) {
	    error("Element %d of y has length %d; expected %d", i, ylen, T);
	}

	PROTECT(Xi = coerceVector(VECTOR_ELT(X,i), REALSXP));
	PROTECT(yi = coerceVector(VECTOR_ELT(y,i), INTSXP));

	cloglik(REAL(Xi), INTEGER(yi), T, m, bp, &loglik, score, info);
	
	UNPROTECT(2);
    }

    /* Construct return value in a form suitable for nlm */

    PROTECT(ans = ScalarReal(-loglik));
    PROTECT(grad = allocVector(REALSXP, m));
    PROTECT(hess = allocVector(REALSXP, M));
    PROTECT(dims = allocVector(INTSXP, 2));

    for (i = 0; i < m; ++i) {
	REAL(grad)[i] = -score[i];
    }
    for (i = 0; i < M; ++i) {
	REAL(hess)[i] = info[i];
    }
    INTEGER(dims)[0] = m;
    INTEGER(dims)[1] = m;

    setAttrib(ans, install("gradient"), grad);
    setAttrib(hess, R_DimSymbol, dims);
    setAttrib(ans, install("hessian"), hess);

    UNPROTECT(4);

    return(ans);
}
