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
    Rcpp::NumericVector maxSolved_design,
    Rcpp::NumericVector probs,
    Rcpp::LogicalVector cumulative)
{
  
  int mm = std::max<int>(eps.size(),rcum[m-1]);
  int b, k, rmax_s, rmax_b;
  int rmax = rcum[m-1] + 1; 
  int ii, rr, kk, ncol, ocol;
  // double* maxelement = std::max_element(minSolved.begin(), minSolved.end());
  // double* maxelement = std::max(minSolved.begin(), minSolved.end());
  int maxelement = Rcpp::max(minSolved);

  Rcpp::NumericVector gamma0(m * rmax);
  Rcpp::NumericVector prob_position(eps_position.size());

  gamma0[0] = probs[0];
 
  // set first block
  for (int g = 0; g < maxSolved[0]; g++) {
    gamma0[g + 1] = eps[g] * probs[g + 1];
  }
 
  if (Rcpp::any(cumulative).is_true()) {
    prob_position[0] = 0;
    for (int i = 1; i < prob_position.size(); i++) {
      prob_position[i] = prob_position[i-1] + maxSolved_design[i-1] + 1;
    }
  } 
 
  for (b = 0; b < m; b++) {
    rmax_s = (rmax - 1) * (b);
    rmax_b = (b + 1) * rmax;

    for (k = rmax_s; k < rmax_b; k++) {
      if ((k < rcum[0]) && (k < minSolved[0])) {
        gamma0[k] = 0;
      } else if (((k % rmax) == 0) && (minSolved[b] == 0) && (b > 0)) {
        if (cumulative[b]) {
          gamma0[k + minSolved_design[b]] = 1.0;
        } else if (maxSolved[b] != 0) {
          gamma0[k + minSolved_design[b]] = probs[eps_position[b] + b];
        }
      }
    }
  }
  
  for (ii = 1; ii < m; ii++) {
    
    ocol = (ii - 1) * rmax;
    ncol = ii * rmax;
  
    for (rr = std::max<int>(minSolved[ii],minSolved_design[ii]); rr <= std::min<int>(rcum[ii],maxSolved_design[ii]); rr++) {

      if ((std::min<int>(minSolved[ii], minSolved_design[ii]) == 0) && (rr <= (rcum[ii-1]))) {
        if (cumulative[ii-1]) {
          gamma0[rr + ncol] = gamma0[rr + ocol];
        } else {
          gamma0[rr + ncol] = gamma0[rr + ocol] * probs[eps_position[ii] + ii]; 
        }
      }
      for (kk = 0; kk < std::min<int>(maxSolved[ii], rr); kk++) {

        if (((rr - (kk + 1) + ocol) <= 0) && (maxelement == 0) && (!cumulative[ii - 1]))
        {
          gamma0[rr + ncol] += eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1] * probs[eps_position[ii - 1] + ii - 1]; 
        } else if ((minSolved_design[ii] == 0) || ((rr >= minSolved_design[ii]) && (kk >= minSolved[ii] - 1)) ) {

          if (cumulative[ii-1]) {
          gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk];
          } else {
            gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1];
          }
        }
      }
    }
    if (cumulative[ii-1]) {
      for (int p = 0; p <= maxSolved_design[ii]; p++)
      {
        gamma0[ncol + p] *= probs[prob_position[ii] + p];
      }
    }
  }
  return gamma0[ Rcpp::Range( ((m * rmax) - mm - 1), (m * rmax) - 1)];
}
#endif
