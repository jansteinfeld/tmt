#ifndef ESF_MST_SUM_VECTOR_s1_H
#define ESF_MST_SUM_VECTOR_s1_H
#include <Rcpp.h>
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
Rcpp::NumericVector esf_mst_sum_vector_s1(
                      Rcpp::NumericVector par,
                      Rcpp::NumericVector oj)
  {

  /* access variables */

  // int npar = par.size();
  int m = oj.size();
  int rmax;
  Rcpp::NumericVector rcum = oj.size();
  Rcpp::NumericVector eps_position = oj.size();
  rcum(0) = oj(0) + 1;		/* +1 adds score zero */
  eps_position(0) = 0;
  for(int i = 1; i < m; i++) {
    rcum(i) = rcum(i-1) + oj(i);
    eps_position(i) = eps_position(i-1) + oj(i-1);
  }
  rmax = rcum(m-1);
  /* loop variables */
  int mm = par.size();
  int i, k, r, ncol, ocol;
  Rcpp::NumericVector gamma0(m * rmax);
  Rcpp::NumericMatrix gamma1(rmax, m);

  /* clear given gamma0 */
  for (k = 0; k < m * rmax; k++) {
    if ((k % rmax) == 0) {
      gamma0[k] = 1.0;
    } else if (k <= oj[0]) {
      gamma0[k] = par[k-1];
    } else {
      gamma0[k] = 0.0;
    }
  }
/* summation algorithm, zero order */
for (i = 1; i < m; i++) {	/* successively add items */
/* calculate column indices once */
  ncol = i * rmax;
  ocol = (i-1) * rmax;
  for (r = 1; r < rcum[i]; r++) {
    gamma0[r + ncol] = gamma0[r + ocol];	 /* already score r with i-1 items or ... */
    for (k = 0; (k < oj[i]) & (k <= r); k++) /* ..score r-k and now cat k (0 means 1). */
      gamma0[r + ncol] += gamma0[r - (k + 1) + ocol] * par[eps_position[i]+k];
    }
  }
  return gamma0[ Rcpp::Range( (m * rmax)-mm, (m * rmax)-1 )];
}
#endif
