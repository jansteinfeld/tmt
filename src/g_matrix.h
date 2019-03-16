#ifndef G_MATRIX_H
#define G_MATRIX_H
#include <Rcpp.h>

Rcpp::NumericVector g_matrix(
  Rcpp::NumericMatrix g_mat, 
  Rcpp::NumericVector epsi_k, 
  int size_mat)
{
  for(int j = 1; j < size_mat; ++j){
    for(int i = 1; (i < size_mat) & (i <= j); ++i){
      if(i == j){
        g_mat(i+1,j) = g_mat(i,j-1) * epsi_k[j];
      }
      g_mat(i+1,j) = g_mat(i+1,j-1) + g_mat(i,j-1) * epsi_k[j];
    }
      //return g_mat;
  }
  return g_mat(Rcpp::_,(size_mat-1));
}
#endif
