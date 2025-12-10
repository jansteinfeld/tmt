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
  // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;

  // set first block
  for (int g = 0; g < maxSolved[0]; g++) {
    // Rcpp::Rcout << "probs: " << probs << std::endl;
    // Rcpp::Rcout << "eps: " << eps << std::endl;
    // Rcpp::Rcout << "g: " << g << std::endl;
    // Rcpp::Rcout << "probs[g + 1]" << probs[g + 1] << std::endl;

    gamma0[g + 1] = eps[g] * probs[g + 1];
  }
  // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;

  if (Rcpp::any(cumulative).is_true()) {
    prob_position[0] = 0;
    for (int i = 1; i < prob_position.size(); i++) {
      prob_position[i] = prob_position[i-1] + maxSolved_design[i-1] + 1;
    }
  } 
  // else {
  //   std::copy(eps_position.begin(),eps_position.end(),prob_position.begin());
  // }

    // Rcpp::Rcout << "------------------------------------" << std::endl;
    // Rcpp::Rcout << "eps: " << eps << std::endl;
    // Rcpp::Rcout << "m: " << m << std::endl;
    // Rcpp::Rcout << "rcum: " << rcum << std::endl;
    // Rcpp::Rcout << "eps_position: " << eps_position << std::endl;
    // Rcpp::Rcout << "minSolved: " << minSolved << std::endl;
    // Rcpp::Rcout << "maxSolved: " << maxSolved << std::endl;
    // Rcpp::Rcout << "minSolved_design: " << minSolved_design << std::endl;
    // Rcpp::Rcout << "maxSolved_design: " << maxSolved_design << std::endl;
    // Rcpp::Rcout << "probs: " << probs << std::endl;
    // Rcpp::Rcout << "cumulative: " << cumulative << std::endl;
    // Rcpp::Rcout << "rmax: " << rmax << std::endl;
    // Rcpp::Rcout << "------------------------------------" << std::endl;
    // Rcpp::Rcout << "prob_position: " << prob_position << std::endl;
    // Rcpp::Rcout << "------------------------------------" << std::endl;

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
          // Rcpp::Rcout << "probs[eps_position[b] + b]: " << probs[eps_position[b] + b] << std::endl;
          // Rcpp::Rcout << "eps_position: " << eps_position << std::endl;
          // Rcpp::Rcout << "b: " << b << std::endl;
          // Rcpp::Rcout << "minSolved_design: " << minSolved_design << std::endl;
          // Rcpp::Rcout << "k: " << k << std::endl;
          // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
        }
      }
    }
  }
  

  // Rcpp::Rcout << "----------------------------------" << std::endl;
  // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
  // Rcpp::Rcout << "rcum: " << rcum << std::endl;
  // Rcpp::Rcout << "minSolved: " << minSolved << std::endl;
  // Rcpp::Rcout << "minSolved_design: " << minSolved_design << std::endl;
  // Rcpp::Rcout << "maxSolved: " << maxSolved << std::endl;
  // Rcpp::Rcout << "maxSolved_design: " << maxSolved_design << std::endl;
  // Rcpp::Rcout << "probs: " << probs << std::endl;
  // Rcpp::Rcout << "eps_position: " << eps_position << std::endl;
  // Rcpp::Rcout << "prob_position: " << prob_position << std::endl;
  // Rcpp::Rcout << "eps: " << eps << std::endl;
  // Rcpp::Rcout << "----------------------------------" << std::endl;

  for (ii = 1; ii < m; ii++) {
    
    ocol = (ii - 1) * rmax;
    ncol = ii * rmax;
    // Rcpp::Rcout << "ii: " << ii << std::endl;
    // Rcpp::Rcout << "ocol: " << ocol << std::endl;
    // Rcpp::Rcout << "ncol: " << ncol << std::endl;
    // Rcpp::Rcout << "rmax: " << rmax << std::endl;

    for (rr = std::max<int>(minSolved[ii],minSolved_design[ii]); rr <= std::min<int>(rcum[ii],maxSolved_design[ii]); rr++) {
//----Rcpp::Rcout << "-----------------------------------" << std::endl;
//----Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
//----Rcpp::Rcout << "-----------------------------------" << std::endl;
      if ((std::min<int>(minSolved[ii], minSolved_design[ii]) == 0) && (rr <= (rcum[ii-1]))) {
        if (cumulative[ii-1]) {
    //----Rcpp::Rcout << "ERROR: " << std::endl;
          gamma0[rr + ncol] = gamma0[rr + ocol];
        } else {
    //----Rcpp::Rcout << "-----------------------------------" << std::endl;
    //----Rcpp::Rcout << "-----------------------------------" << std::endl;
    //----Rcpp::Rcout << "gamma0 a1): " << gamma0 << std::endl;
    //----Rcpp::Rcout << "eps_position: " << eps_position << std::endl;
    //----Rcpp::Rcout << "probs[eps_position[ii] + ii]: " << probs[eps_position[ii] + ii] << std::endl;
    //----Rcpp::Rcout << "eps_position[ii] + ii + 1: " << eps_position[ii] + ii + 1<< std::endl;
          // Rcpp::Rcout << "ii: " << ii << std::endl;
          // Rcpp::Rcout << "rr: " << rr << std::endl;
    //----Rcpp::Rcout << "gamma0[rr + ocol]: " << gamma0[rr + ocol] << std::endl;
    //----Rcpp::Rcout << "gamma0[rr + ncol]: " << gamma0[rr + ncol] << std::endl;
          // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
          gamma0[rr + ncol] = gamma0[rr + ocol] * probs[eps_position[ii] + ii]; // 
    //----Rcpp::Rcout << "gamma0 b1): " << gamma0 << std::endl;
          // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
    //----Rcpp::Rcout << "-----------------------------------" << std::endl;
    //----Rcpp::Rcout << "-----------------------------------" << std::endl;
        }
      }
      for (kk = 0; kk < std::min<int>(maxSolved[ii], rr); kk++) {

        // if (((rr - (kk + 1) + ocol) <= 0) && (std::max_element(minSolved.begin(), minSolved.end()) == 0) ) {
        // Rcpp::Rcout << "ii: " << ii << std::endl;
        // Rcpp::Rcout << "rr: " << rr << std::endl;
        // Rcpp::Rcout << "kk+1: " << kk + 1 << std::endl;
        // Rcpp::Rcout << "ocol: " << ocol << std::endl;
        // Rcpp::Rcout << "maxelement: " << maxelement << std::endl;
        // // Rcpp::Rcout << "maxelement2: " << maxelement2 << std::endl;
        // Rcpp::Rcout << "(rr - (kk + 1) + ocol): " << (rr - (kk + 1) + ocol) << std::endl;
        if (((rr - (kk + 1) + ocol) <= 0) && (maxelement == 0) && (!cumulative[ii - 1]))
        {
          // Rcpp::Rcout << "if (((rr - (kk + 1) + ocol) <= 0) && (maxelement == 0) ) {"<< std::endl;
    //----Rcpp::Rcout << "probs: " << probs << std::endl;
    //----Rcpp::Rcout << "probs[eps_position[ii] + ii + kk + 1] " << probs[eps_position[ii] + ii + kk + 1]<< std::endl;
    //----Rcpp::Rcout << "probs[eps_position[ii - 1] + ii - 1]  " << probs[eps_position[ii - 1] + ii - 1] << std::endl;
    //----Rcpp::Rcout << "eps_position[ii]: " << eps_position[ii] << std::endl;
    //----Rcpp::Rcout << "ii: " << ii << std::endl;
    //----Rcpp::Rcout << "kk: " << kk << std::endl;
    //----Rcpp::Rcout << "eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1] * probs[eps_position[ii - 1] + ii - 1]): " << eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1] * probs[eps_position[ii - 1] + ii - 1] << std::endl;
    //----Rcpp::Rcout << "eps[eps_position[ii] + kk]: " << eps[eps_position[ii] + kk] << std::endl;

          gamma0[rr + ncol] += eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1] * probs[eps_position[ii - 1] + ii - 1];
          
    //----Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;

        } else if ((minSolved_design[ii] == 0) || ((rr >= minSolved_design[ii]) && (kk >= minSolved[ii] - 1)) ) {

          // if (cumulative[ii-1]) {
          // Rcpp::Rcout << "eps[eps_position[ii] + kk]: " << eps[eps_position[ii] + kk] << std::endl;
    
          // Rcpp::Rcout << "-----------------------------------" << std::endl;
          // Rcpp::Rcout << "gamma0 a): " << gamma0 << std::endl;
          // Rcpp::Rcout << "eps: " << eps << std::endl;
          // Rcpp::Rcout << "probs: " << probs << std::endl;
          // Rcpp::Rcout << "ii: " << ii << std::endl;
          // Rcpp::Rcout << "kk: " << kk << std::endl;
          // Rcpp::Rcout << "eps[eps_position[ii] + kk]: " << eps[eps_position[ii] + kk] << std::endl;
          // Rcpp::Rcout << "eps_position[ii]: " << eps_position[ii] + kk << std::endl;
          // Rcpp::Rcout << "probs[eps_position[ii] + ii + kk + 1]: " << probs[eps_position[ii] + ii + kk + 1] << std::endl;
          // Rcpp::Rcout << "eps_position[ii] + ii + kk + 1: " << eps_position[ii] + ii + kk + 1 << std::endl;
          // Rcpp::Rcout << "probs[prob_position[ii] + kk + 1]: " << probs[prob_position[ii] + kk + 1] << std::endl;
          // Rcpp::Rcout << "prob_position[ii] + kk + 1: " << prob_position[ii] + kk + 1 << std::endl;
          // Rcpp::Rcout << "prob_position[ii]: " << prob_position[ii] << std::endl;
          // Rcpp::Rcout << "prob_position: " << prob_position << std::endl;
          // Rcpp::Rcout << "gamma0[rr - (kk + 1) + ocol]: " << gamma0[rr - (kk + 1) + ocol] << std::endl;
          // Rcpp::Rcout << "rr - (kk + 1) + ocol: " << rr - (kk + 1) + ocol << std::endl;
          // Rcpp::Rcout << "rr + ncol: " << rr + ncol << std::endl;
          
          if (cumulative[ii-1]) {
            
            
            // hier die wahrscheinlichkeiten anpassen

            // gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk] * probs[prob_position[ii] + kk + 1];
          gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk];
          } else {
            gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1];
          }

          // Rcpp::Rcout << "gamma0 b): " << gamma0 << std::endl;
          // Rcpp::Rcout << "-----------------------------------" << std::endl;

    //----Rcpp::Rcout << "gamma0 b): " << gamma0 << std::endl;
    //----Rcpp::Rcout << "-----------------------------------" << std::endl;
          // } else {
            // gamma0[rr + ncol] += gamma0[rr - (kk + 1) + ocol] * eps[eps_position[ii] + kk] * probs[eps_position[ii] + ii + kk + 1];
          // }
        }
        // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
      }
    }
    if (cumulative[ii-1]) {
      for (int p = 0; p <= maxSolved_design[ii]; p++)
      {
        
        // Rcpp::Rcout << "probs  " << probs << std::endl;
        // Rcpp::Rcout << "probs[prob_position[ii] + p]: " << probs[prob_position[ii] + p] << std::endl;
        // Rcpp::Rcout << "prob_position  " << prob_position << std::endl;
        // Rcpp::Rcout << "prob_position[ii] + p  " << prob_position[ii] + p << std::endl;
        // Rcpp::Rcout << "ii  " << ii << std::endl;
        // Rcpp::Rcout << "ii  " << ii << std::endl;
        // Rcpp::Rcout << "p  " << p << std::endl;
        gamma0[ncol + p] *= probs[prob_position[ii] + p];
      }
    }
  }
  
  // Rcpp::Rcout << "m: " << m << " rmax: " << rmax << " mm: " << mm << " gamma0: " << gamma0 << std::endl;
  // Rcpp::Rcout << "----------------------------------" << std::endl;
  // Rcpp::Rcout << "gamma0: " << gamma0 << std::endl;
  return gamma0[ Rcpp::Range( ((m * rmax) - mm - 1), (m * rmax) - 1)];
}
#endif
