#include <Rcpp.h>
#include "g_matrix.h"
using namespace Rcpp;

// Calculation of the Basic-Symmetric-Function (summation)
// Verhelst, N.D., Glas,C.A.W. und van der Sluis, A. (1984).
// Estimation Problems in the Rasch-Model: The Basic Symmetric Functions.
// Computational Statistics Quatarly, 1(3), 245-262.
// Equation (8)
// [[Rcpp::export]]
List esf_nmst_sum_matrix(NumericVector epsi, int order=0){

  // ---------------------------
  // define variables:
  // ---------------------------
  // output list
    List g_list;
  // size of matrix
    int size_mat = epsi.size();
  // define vector and matrix
    NumericVector epsi_k(size_mat);
    NumericMatrix g_mat( size_mat+1, size_mat );
  // ---------------------------
  // loop for exp(-epsi)
    for(int i = 0; i < size_mat; i++){
      epsi_k(i) = exp(-epsi(i));
    }
  // ---------------------------

  // START
  if((order == 0) | (order == 1) | (order == 2)){ // condition for first order

    // faster solution for sugar-version of cumsum
    NumericVector epsiSum(epsi_k.size());
    std::partial_sum(epsi_k.begin(), epsi_k.end(), epsiSum.begin());

    //NumericVector epsiSum = cumsum( epsi_k );
    g_mat(0,size_mat-1) = 1;
    g_mat(1,_) = epsiSum;

    // call rcpp function g_matrix
    g_list["0"] = g_matrix(g_mat, epsi_k, size_mat);

  }

  if((order == 1) | (order == 2)){ // condition for first order
    // Initialize output vector only once 2018-01-06
    NumericVector epsi_ki( size_mat );
    NumericMatrix g_mat2( size_mat+1, size_mat );


    for(int k=0; k < size_mat; ++k){

      // loop over epsi_k vector
      int iii=1;
      epsi_ki[0] = 0;
      for (int ii = 0; ii < size_mat; ii++) {
        if(ii!=k){
          epsi_ki[iii] = epsi_k[ii];
          iii = iii+1;
        }
        g_mat2(1,ii) = epsi_k[k];
      }

      // call rcpp function g_matrix
      g_mat(_,k) = g_matrix(g_mat2, epsi_ki, size_mat);

      // clear object for reuse
      std::fill(g_mat2.begin(), g_mat2.end(), 0);
      std::fill(epsi_ki.begin(), epsi_ki.end(), 0);
    }
    g_list["1"] = g_mat;
  }

  if(order == 2){ // condition for second order
    // Initialize output vector only once 2018-12-06

    NumericVector epsi_ki2( size_mat );
   
    NumericVector epsi_k2(epsi_k.size()+2);
    int epsi_k_size = epsi_k.size();
    epsi_k2[0] = 0.0;
    epsi_k2[1] = 0.0;
    std::copy(epsi_k.begin(),epsi_k.end(),epsi_k2.begin() + 2);

    NumericVector g_vec3((epsi_k_size+1)*epsi_k_size*epsi_k_size);

    IntegerVector dim_2o = IntegerVector::create((epsi_k_size+1),epsi_k_size,epsi_k_size);

    int iii = 0;
    for(int i = 0; i < epsi_k2.size(); i++){
      for(int j = i+1; j < epsi_k2.size()-1; j++){
        NumericMatrix g_mat3( size_mat + 1, size_mat);
        int ii = 1;
        epsi_ki2[0] = 0.0;
        for (int k = 0; k < epsi_k2.size()-1; k++) {
          if((i!=k) & (j!=k)){
            epsi_ki2[ii] = epsi_k2[k+1];
            ii++;
          }
        }
        for(int kk = 0; kk < size_mat; kk++){
          if(i > 0){
            g_mat3(2,kk) = epsi_k2[j+1] * epsi_k2[i+1];
          } else {
            g_mat3(1,kk) = epsi_k2[j+1];
          }
        }

      NumericVector epsi_k2out(epsi_k2.size());
      // call rcpp function g_matrix
      epsi_k2out = g_matrix(g_mat3, epsi_ki2, size_mat);
      int epsi_k2out_size = epsi_k2out.size();
      if(i == 0){
        std::copy(epsi_k2out.begin(), epsi_k2out.end(), g_vec3.begin() + (epsi_k2out_size * epsi_k2out_size * (j - 1)) + iii);
      } else {
        std::copy(epsi_k2out.begin(), epsi_k2out.end(), g_vec3.begin() + (epsi_k2out_size * epsi_k2out_size * (i - 1) + ((j - i) * epsi_k2out_size)));
        std::copy(epsi_k2out.begin(), epsi_k2out.end(), g_vec3.begin() + (epsi_k2out_size * epsi_k2out_size * (j - 1) + ((i - j) * epsi_k2out_size)));
      }
        std::fill(epsi_k2out.begin(), epsi_k2out.end(), 0);
      } //end j
      iii += epsi_k2.size() - 1;

    }//end i
    g_vec3.attr("dim") = dim_2o;
    g_list["2"] = g_vec3;
  }
 // return list
  return g_list;
}

