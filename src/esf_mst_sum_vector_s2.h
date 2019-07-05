#ifndef ESF_MST_SUM_VECTOR_s2_H
#define ESF_MST_SUM_VECTOR_s2_H
#include <Rcpp.h>
#include <numeric>      // std::accumulate
// Here we define the second helper function for the mst-design.
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::export]]
Rcpp::NumericVector esf_mst_sum_vector_s2(
    Rcpp::NumericVector eps,
    int m,
    Rcpp::NumericVector rcum,
    Rcpp::NumericVector eps_position,
    Rcpp::NumericVector minSolved,
    Rcpp::NumericVector maxSolved,    
    Rcpp::NumericVector minSolved_design,
    Rcpp::NumericVector maxSolved_design)
{
  /* loop variables */
  int mm = eps.size();
  int b, k, rmax_s, rmax_b;
  int rmax = rcum[m-1] + 1;
  Rcpp::NumericVector gamma0(m * rmax);
  gamma0[0] = 1;
  // set first block
  for(int g = 1; g <= maxSolved[0]; g++){
    gamma0[g] = eps[g-1];
  }

 for (b = 0; b < m; b++) {
    rmax_s = (rmax - 1) * (b);
    rmax_b = (b + 1) * rmax;

    //double prod = 1.0;
    for (k = rmax_s; k < rmax_b; k++) {
      if ((k < rcum[0]) & (k < minSolved[0])) {
        gamma0[k] = 0;
      } else if (((k % rmax) == 0) & (minSolved[b] == 0) && (b > 0)) {
          gamma0[k + minSolved_design[b]] = 1.0;
      }
    }
  }


  int ii, rr, kk, ncol, ocol;
  for (ii = 1; ii < m; ii++) {
    ocol = (ii - 1) * rmax;
    ncol = ii * rmax;
    for (rr = std::max<int>(minSolved[ii],minSolved_design[ii]); rr <= std::min<int>(rcum[ii],maxSolved_design[ii]); rr++) {
      if (std::min<int>(minSolved[ii], minSolved_design[ii]) == 0) {
        gamma0[rr + ncol] = gamma0[rr + ocol];
      }
      for (kk = 0; kk < std::min<int>(maxSolved[ii], rr); kk++) {
        if (((rr - (kk + 1) + ocol) <= 0) & (Rcpp::max(minSolved) == 0) ) {
          gamma0[rr + ncol] += eps[eps_position[ii] + kk];
        } else if ((minSolved_design[ii] == 0) | ((rr >= minSolved_design[ii]) & (kk >= minSolved[ii] - 1)) ) {
          gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk];
        }
      }
    }
  }
  // Rcpp::Rcout << "gamma0           " << gamma0 << std::endl;
return gamma0[ Rcpp::Range( (m * rmax)-mm - 1, (m * rmax) - 1)];
}
#endif
