#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/* 
   Utility functions by Terry Therneau from the survival package 
   These are specially adapted to work with information matrices that
   are not of full rank.
 */
int cholesky2(double **matrix, int m, double toler);
void chsolve2(double **matrix, int m, double *y);
void chinv2(double **matrix, int m);

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
static void cloglik_stratum(double const *X, int const *y, int T, int m, 
			    double const *beta, double *loglik, double *score, 
			    double *info)
{
    double *f, *g, *h, *xt, *lpmax, *xmean;
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
    if (2 * K > T/2) {
	K = T - K;
	iscase = 0;
	sign = -1;
    }

    /*
      Calculate the maximum value of the linear predictor (lpmax) within the
      stratum. This is subtracted from the linear predictor when taking
      exponentials for numerical stability. Note that we must correct
      the log-likelihood for this, but not the score or information matrix.
    */
    lpmax = Calloc(m, double);
    for (i = 0; i < m; ++i) {
	lpmax[i] = beta[i] * sign * X[T*i];
    }
    for (t = 1; t < T; ++t) {
	for (i = 0; i < m; ++i) {
	    double lp = beta[i] * sign * X[t + T*i];
	    if (lp > lpmax[i])
		lpmax[i] = lp;
	}
    }
    /* 
       Calculate the mean value of the covariates within the stratum.
       This is used to improve the numerical stability of the score
       and information matrix.
    */
    xmean = Calloc(m, double);
    for (i = 0; i < m; ++i) {
	xmean[i] = 0;
	for (t = 0; t < T; ++t) {
	    xmean[i] += sign * X[t + T*i];
	}
	xmean[i] /= T;
    }

    /* Contribution from cases */
    for (t = 0; t < T; ++t) {
	if (y[t] == iscase) {
	    for (i = 0; i < m; ++i) {
		loglik[0] += sign * X[t + i*T] * beta[i] - lpmax[i];
		score[i] += sign * X[t + i*T] - xmean[i];
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
       Recursively calculate contributions over all possible case sets
       of size K.
    */

    for (t = 0; t < T; ++t) {

	double Ct = 0;
	for (i = 0; i < m; ++i) {
	    xt[i] = sign * X[t + T*i];
	    Ct += beta[i] * xt[i] - lpmax[i];
	    xt[i] -= xmean[i];
	}
	Ct = exp(Ct);

	for (k = imin2(K,t+1); k > 0; --k) {

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

    /* Add contributions from this stratum to the output parameters */

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
    Free(lpmax);
    Free(xmean);
}

/*
 * Calculate the conditional log likelihood summed over all strata,
 * along with the score and information matrix.
 *
 * Input parameters:
 *
 * X    - list of matrices of covariate values. One element of the list
 *        corresponds to a single stratum
 * Y    - list of vectors of outcomes, corresponding to X,
 * beta - vector of log odds ratio parameters 
 * m    - number of parameters
 *
 * Output parameters
 *
 * loglik - contains the conditional log-likelihood on exit (scalar)
 * score  - contains the score function on exit (m - vector)
 * info   - contains the information matrix on exit (m*m - vector)
 */

static void cloglik(SEXP X, SEXP y, int m, double *beta, 
		    double *loglik, double *score, double *info)
{
    int i;
    int M = m*m;

    /* Output parameters of cloglik_stratum must be initialized to zero */
    loglik[0] = 0;
    for (i = 0; i < m; ++i) {
	score[i] = 0;
    }
    for (i = 0; i < M; ++i) {
	info[i] = 0;
    }
    
    for (i = 0; i < length(X); ++i) {
	SEXP Xi = VECTOR_ELT(X,i);
	SEXP yi = VECTOR_ELT(y,i);
	cloglik_stratum(REAL(Xi), INTEGER(yi), nrows(Xi), m, beta, 
			loglik, score, info);
    }
}

/* 
   The chinv2 function only works on the lower triangle of the matrix.
   This wrapper function copies the lower to the upper triangle
*/
static void invert_info(double **imat, int m)
{
    int i,j;

    chinv2(imat, m);
    for (i = 1; i < m; i++) {
	for (j = 0; j < i; j++) {
	    imat[i][j] = imat[j][i];
	}
    }
}

/* 
   Find maximum likelihood estimate of conditional logistic regression
   model by Newton-Raphson. The algorithm is copied from coxph.fit
   from the survival package by Terry Therneau.

   The variable u is used to store both the score function and the
   step for the next iteration. Likewise info contains both the information
   matrix and its Cholesky decomposition. If flag > 0 then the arrays
   hold the score and variance-covariance matrix respectively.
*/

//FIXME: weights, offset
static void clogit_fit(SEXP X, SEXP y, int m, double *beta, 
		       double *loglik, double *u, double *info, 
		       int *flag, int *maxiter, double const *eps, 
		       double const * tol_chol)
{
    int i, iter = 0;
    Rboolean halving = FALSE;
    double *oldbeta = Calloc(m, double);
    double **imat = Calloc(m, double*);

    /* 
       Set up ragged array representation of information matrix for
       use by cholesky2, chsolve2, and invert_info functions 
    */
    for (i = 0; i < m; ++i) {
	imat[i] = info + m*i;
    }

    /* Initial iteration */

    cloglik(X, y, m, beta, loglik, u, info);
    if (*maxiter > 0) {
	*flag = cholesky2(imat, m, *tol_chol);
	if (*flag > 0) {
	    chsolve2(imat, m, u);
	    for (i = 0; i < m; i++) {
		oldbeta[i] = beta[i];
		beta[i] += u[i];
	    }
	}
	else {
	    /* Bad information matrix. Don't go into the main loop */
	    *maxiter = 0;
	}
    }
    
    /* Main loop */
    for (iter = 1; iter <= *maxiter; iter++) {

	double oldlik = *loglik;
	cloglik(X, y, m, beta, loglik, u, info);

	if (fabs(1 - (oldlik / *loglik)) <= *eps && !halving) {
	    /* Done */
	    break;
	}
	else if (iter == *maxiter) {
	    /* Out of time */
	    *flag = 1000;
	    break;
	}
	else if (*loglik < oldlik) {    
	    /* Not converging: halve step size */
	    halving = TRUE;
	    for (i = 0; i < m; i++) {
		beta[i] = (beta[i] + oldbeta[i]) /2;
	    }
	}
	else {
	    /* Normal update */
	    halving = FALSE;
	    oldlik = *loglik;
	    *flag = cholesky2(imat, m, *tol_chol);
	    if (*flag > 0) {
		chsolve2(imat, m, u);
		for (i = 0; i < m; i++) {
		    oldbeta[i] = beta[i];
		    beta[i] += u[i];
		}
	    }
	    else {
		break; /* Bad information matrix */
	    }
	}
    }

    *maxiter = iter;
    if (*flag > 0) {
	cholesky2(imat, m, *tol_chol);
	invert_info(imat, m);
    }

    Free(oldbeta);
    Free(imat);
}

/* R interface */

SEXP clogit(SEXP X, SEXP y, SEXP coef, SEXP maxiter, SEXP eps, SEXP tol_chol)
{
    int i;
    int n = length(X);
    int m = length(coef);
    int M = m*m;
    int flag = 0;
    int niter = INTEGER(maxiter)[0];
    double loglik[2], *score, *info, *beta;
    SEXP ans, a, names, dims;

    if (!isNewList(X)) error("'X' must be a list");
    if (!isNewList(y)) error("'y' must be a list");
    if (length(X) != length(y)) error("length mismatch between X and y");

    for (i = 0; i < n; ++i) {

	int T = nrows(VECTOR_ELT(X,i));
        int xcols = ncols(VECTOR_ELT(X,i));
        int ylen  = length(VECTOR_ELT(y,i));

	if (xcols != m) {
	    error("Element %d of X has %d columns; expected %d", i, xcols, m);
	}
	if (ylen != T) {
	    error("Element %d of y has length %d; expected %d", i, ylen, T);
	}

    }

    beta = (double *) R_alloc(m, sizeof(double));
    for (i = 0; i < m; ++i) {
	beta[i] = REAL(coef)[i];
    }
    score = (double *) R_alloc(m, sizeof(double));
    info = (double *) R_alloc(M, sizeof(double));

    /* Calculate null loglikelihood */
    cloglik(X, y, m, beta, &loglik[0], score, info);
    
    /* Maximize the likelihood */
    clogit_fit(X, y, m, beta, &loglik[1], score, info,
	       &flag, &niter, REAL(eps), REAL(tol_chol));

    /* Construct return list */

    PROTECT(ans = allocVector(VECSXP, 5));
    PROTECT(names = allocVector(STRSXP, 5));

    /* Estimates */
    PROTECT(a = allocVector(REALSXP, m));
    for (i = 0; i < m; ++i) {
	REAL(a)[i] = beta[i];
    }
    SET_VECTOR_ELT(ans, 0, a);
    SET_STRING_ELT(names, 0, mkChar("coefficients"));
    UNPROTECT(1);

    /* Log likelihood */
    PROTECT(a = allocVector(REALSXP, 2));
    REAL(a)[0] = loglik[0];
    REAL(a)[1] = loglik[1];
    SET_VECTOR_ELT(ans, 1, a);
    SET_STRING_ELT(names, 1, mkChar("loglik"));
    UNPROTECT(1);

    /* Information matrix */
    PROTECT(a = allocVector(REALSXP, M));
    PROTECT(dims = allocVector(INTSXP, 2));
    for (i = 0; i < M; ++i) {
	REAL(a)[i] = info[i];
    }
    INTEGER(dims)[0] = m;
    INTEGER(dims)[1] = m;
    setAttrib(a, R_DimSymbol, dims);    
    SET_VECTOR_ELT(ans, 2, a);
    SET_STRING_ELT(names, 2, mkChar("var"));
    UNPROTECT(2);

    /* Flag */
    PROTECT(a = ScalarInteger(flag));
    SET_VECTOR_ELT(ans, 3, a);
    SET_STRING_ELT(names, 3, mkChar("flag"));
    UNPROTECT(1);

    /* Number of iterations */
    PROTECT(a = ScalarInteger(niter));
    SET_VECTOR_ELT(ans, 4, a);
    SET_STRING_ELT(names, 4, mkChar("iter"));
    UNPROTECT(1);

    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return(ans);
}
