#ifndef ESF_MST_SUM_VECTOR_s2_H
#define ESF_MST_SUM_VECTOR_s2_H
#include <Rcpp.h>
#include <numeric>      // std::accumulate
// Here we define the second helper function for the mst-design. Within this function the
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
    Rcpp::NumericVector maxSolved)
{
  /* loop variables */
  int mm = eps.size();
  int b, k, rmax_s, rmax_b;
  int rmax = rcum[m-1] + 1;
  Rcpp::NumericVector gamma0(m * rmax);
// set first block
for(int g = 1; g <= maxSolved[0]; g++){
  gamma0[g] = eps[g-1];
}


for (b = 0; b < m; b++) {
  rmax_s = (rmax - 1) * (b);
  rmax_b = (b + 1) * rmax;
  double prod = 1.0;
  for (k = rmax_s; k < rmax_b; k++) {
    if ((k < rcum[0]) & (k < minSolved[0])) {
      gamma0[k] = 0;
    } else if ((k % rmax) == 0) {
      if (minSolved[b] != 0 && b > 0) {
        // helper function for adding up the eps if more than one block has minSolved > 0
        for (int i = 0; i < minSolved.size(); i++) {
          if(minSolved[i] > 0){
            prod *= eps[eps_position[i] + minSolved[i] - 1];
          }
        }
        // end helper function
        int sumrange = std::accumulate(minSolved.begin(), minSolved.begin() + (b + 1), 0);
        gamma0[k + sumrange] = prod;
        prod = 0.0;
      } else {
        int sumrange = std::accumulate(minSolved.begin(), minSolved.begin() + (b), 0);
        gamma0[k + sumrange] = 1.0;
      }
    }
  }
}

int ii, rr, kk, ncol, ocol;
for (ii = 1; ii < m; ii++) {
  ncol = ii * rmax;
  ocol = (ii - 1) * rmax;
  for (rr = 1; rr <= rcum[ii]; rr++) {
    if (minSolved[ii] == 0) {
      gamma0[rr + ncol] = gamma0[rr + ocol];
    }
    for (kk = 0; kk < std::min<int>(maxSolved[ii], rr); kk++) {
      if (((rr - (kk + 1) + ocol) <= 0) & (Rcpp::max(minSolved) == 0) ) {
        gamma0[rr + ncol] += eps[eps_position[ii] + kk];
      } else {
        if (minSolved[ii] == 0) {
          gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk];
        } else if (rr > sum(minSolved[Rcpp::Range(0,ii)])) {
          if (kk < minSolved[ii] - 1) {
          } else {
            gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk];
          }
        }
      }
    }
  }
}
return gamma0[ Rcpp::Range( (m * rmax)-mm - 1, (m * rmax) - 1)];
}
#endif
