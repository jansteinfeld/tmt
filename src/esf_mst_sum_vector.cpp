#include <Rcpp.h>
using namespace Rcpp;
#include "esf_mst_sum_vector_s1.h"
#include "esf_mst_sum_vector_s2.h"
#include "esf_mst_sum_helperfunctions.h"
// [[Rcpp::export]]
List esf_mst_sum_vector(List parlist, List ojlist, int order, NumericVector minSolved, NumericVector maxSolved)
{
  int psize = parlist.size();
  NumericVector size(psize+1);
  NumericVector sizeIndex(psize+1);
  List eps(psize);

  sizeIndex[0] = 0;
  for(int pp = 1; pp <= psize; pp++){
    NumericVector par = parlist[pp - 1];
    NumericVector eps_tmp(par.size());
    for(int ii = 0; ii < par.size();ii++){
      eps_tmp[ii] = exp(-par(ii));
    }
    eps[pp-1] = eps_tmp;
    size(pp) = par.size();
    sizeIndex(pp) = sizeIndex(pp-1) + size(pp) - 1;
  }

  int dim = sum(size);
  List esf_mst;
  /*
  * ----------------------------------------------
  * Start here
  * ----------------------------------------------
  */
  if((order == 0) | (order == 1) | (order == 2)){

    NumericVector outvector(dim);
    int npar = size.size();
    int m = eps.size();
    NumericVector eps_position(npar);
    NumericVector oj(npar);
    NumericVector rcum(m);

    for(int p = 0; p < psize; p++){
      NumericVector par = eps[p];
      NumericVector oj = ojlist[p];
      Rcpp::NumericVector out = par.size();
      out = esf_mst_sum_vector_s1(par, oj); // erste Funktion anwenden
      outvector[Rcpp::Range( sizeIndex(p) + p, sizeIndex(p + 1) + p )] = out;
    }
    oj = combine(ojlist);
    NumericVector ojlist_i = ojlist[0];
    rcum(0) = ojlist_i.size();
    eps_position(0) = 0;

    for(int i = 1; i < m; i++) {
      NumericVector ojlist_i = ojlist[i-1];
      NumericVector ojlist_m = ojlist[i];
      rcum(i) = rcum(i-1) + ojlist_m.size();
      eps_position(i) = eps_position(i-1) + ojlist_i.size();
    }
    esf_mst["0"] = esf_mst_sum_vector_s2(outvector, m, rcum, eps_position, minSolved, maxSolved); // zweite Funktion anwenden
  }

  // --------------------------------------------------------------------------------------------------------------------
  /*
  * ----------------------------------------------
  * Start for first order here
  * ----------------------------------------------
  */
  if((order == 1) | (order == 2)){
    NumericMatrix esf_mat(dim + 1, dim);
    NumericVector size(psize);
    NumericVector sizeIndex(psize + 1);
    int m = eps.size();
    int k = 0; //helper index
    NumericVector minSolved_l = Rcpp::clone(minSolved);
    NumericVector maxSolved_l = Rcpp::clone(maxSolved);
    NumericVector rcum(m);
    NumericVector eps_position(m);
    NumericVector ojlist_i = ojlist[0];
    rcum(0) = ojlist_i.size() - 1;
    eps_position(0) = 0;
    for(int i = 1; i < m; i++) {
      NumericVector ojlist_i = ojlist[i - 1];
      NumericVector ojlist_m = ojlist[i];
      rcum(i) = rcum(i - 1) + ojlist_m.size();
      if(i < 2){
        eps_position(i) = eps_position(i-1) + ojlist_i.size() - 1;
      }else{
        eps_position(i) = eps_position(i-1) + ojlist_i.size();
      }
    }
    for(int l = 0; l < psize; l++){
      sizeIndex[0] = 0;
      for(int pp = 1; pp <= psize; pp++){
        NumericVector par = eps[pp - 1];
        size(pp - 1) = par.size();
        sizeIndex(pp) = sizeIndex(pp - 1) + size(pp - 1) - 1;
      }
      NumericVector eps_vec(sum(size));
      int index = 0;
      for(int v = 0; v < m; v++){
        NumericVector el = eps[v];
        std::copy(el.begin(), el.end(), eps_vec.begin() + index);
        index += el.size();
      }
      maxSolved_l[l] = maxSolved[l]-1;
      size[l] = size[l]-1;
     if(minSolved[l] > 0) {
       minSolved_l[l] = minSolved[l]-1;
      }
      if(l > 0){
        maxSolved_l[l-1] = maxSolved[l-1];
        size[l-1] = size[l-1] + 1;
        if(minSolved[l-1] > 0){
          minSolved_l[l-1] = minSolved[l-1];
        }
        eps_position[l] = eps_position[l] + 1;
        rcum[l-1] = rcum[l-1]+1;
      }
      NumericVector outvector(dim-1);
      for(int v = 0; v < size[l]+1; v++){
        int sizeV = 0;
        for(int p = 0; p < psize; p++){
          NumericVector par_i = eps[p];
          NumericVector oj_i = ojlist[p];
          NumericVector par(size[p]);
          NumericVector oj(size[p]);
          if (p == l){
            int iii=0;
            for (int ii = 0; ii < par_i.size(); ii++) {
              if(ii!=v){
                par[iii] = par_i[ii];
                oj[iii] = oj_i[ii];
                iii++;
              }
            }
          }else{
            par = eps[p];
            oj = ojlist[p];
          }
          NumericVector out = par.size();
          out = esf_mst_sum_vector_s1(par, oj); // erste Funktion anwenden
          std::copy(out.begin(), out.end(), outvector.begin() + sizeV);
          std::fill(out.begin(), out.end(), 0);// clear object for reuse
          sizeV += out.size();
        }
        NumericVector out(outvector.size()+2);
        out = esf_mst_sum_vector_s2(outvector, m, rcum, eps_position, minSolved_l, maxSolved_l); // zweite Funktion anwenden
        for(int o = 0; o < out.size(); o++){
          out[o] *=  eps_vec[k];
        }
        for(int r = 0; r < out.size(); r++){
          esf_mat(r + 1, k) = out[r];
        }
        k++;//helper index
        std::fill(outvector.begin(), outvector.end(), 0);// clear object for reuse
        std::fill(out.begin(), out.end(), 0);// clear object for reuse
      }
    }
    esf_mst["1"] = esf_mat;
  }

  // --------------------------------------------------------------------------------------------------------------------
  /*
  * ----------------------------------------------
  * Start for second order here
  * ----------------------------------------------
  */
  if(order == 2){
    
    NumericMatrix esf_mat(dim + 1, dim);
    NumericVector minSolved_l = Rcpp::clone(minSolved);
    NumericVector maxSolved_l = Rcpp::clone(maxSolved);
    NumericVector ojlist_i = ojlist[0];
    IntegerVector helper_stages(dim);
    NumericVector eps_vec(dim);
    NumericVector g_vec3((dim+1)*(dim)*(dim));
    //Rcpp::Rcout << "g_vec3: " << g_vec3 << std::endl;
    IntegerVector dim_2o = IntegerVector::create((dim+1),(dim),(dim));
    int m = eps.size();
    int k = 0.0;
    int kk = 0;
    for(int i = 0; i < m; i++) {
      NumericVector eps_i = eps[i];
      for(int ii = 0; ii < eps_i.size(); ii++){
        helper_stages[k] = i;
        eps_vec[k] = eps_i[ii];
        k++;
      }
    }
  for(int j = 0; j < helper_stages.size(); j++){
      for(int v = j; v < helper_stages.size(); v++){ //alle Bloecke in der Liste
          int sizeV = 0;
          int pp = 0;
          int mm = m;
            for (int i = 0; i < psize; i++){
              maxSolved_l[i] = maxSolved[i];
              minSolved_l[i] = minSolved[i];
            }
            if(j > 0){
              NumericVector tmp_eps = eps[helper_stages[j-1]];
              if((j == v) && (tmp_eps.size()<=2) && (j % tmp_eps.size()==1)){
                mm = m-1;
              }
            }
          NumericVector rcum(mm);
          NumericVector eps_position(mm);
          NumericVector minSolved_ll(mm);
          NumericVector maxSolved_ll(mm);
          NumericVector outvector(dim-1);

        for(int p = 0; p < psize; p++){
          NumericVector par_ii = eps[p];
          NumericVector oj = ojlist[p];
          int size_par_i = par_ii.size();

            if(helper_stages[v] == p){
              size_par_i -= 1.0;
            } 
            if((j > 0) && (helper_stages[j-1] == p)){
              size_par_i -= 1.0;
            }           
        if(size_par_i > 0){
          NumericVector par_i(size_par_i);
          NumericVector oj_i(size_par_i);
          int ii = 0;
          if(size_par_i != par_ii.size()){
            for (int i = 0; i < par_ii.size(); i++) {
              if((helper_stages[v] == p) && (v % par_ii.size() == i)){
                maxSolved_l[p] -= 1;
                if(minSolved_l[p] > 0) {
                  minSolved_l[p] -= 1;
                }
              } else if((j > 0) && (helper_stages[j-1] == p) && ((j-1) % par_ii.size() == i)){
                maxSolved_l[p] -= 1;
                if(minSolved_l[p] > 0) {
                  minSolved_l[p] -= 1;
                }
              }else{
                par_i[ii] = par_ii[i];
                oj_i[ii] = oj[i];
                ii++;
              }
            }
          }else{
              par_i = par_ii;
              oj_i = oj;
            }
            // define helper variables for the second stage
              if(pp == 0){
                rcum[pp] = par_i.size();
                eps_position[pp] = 0.0;
              }else{
                rcum[pp] = rcum[pp-1] + par_i.size();
                eps_position[pp] = rcum[pp-1];
              }
              minSolved_ll[pp] = minSolved_l[p];
              maxSolved_ll[pp] = maxSolved_l[p];
              pp++;
              Rcpp::NumericVector out = par_i.size();
              out = esf_mst_sum_vector_s1(par_i, oj_i); // erste Funktion anwenden
              std::copy(out.begin(), out.end(), outvector.begin() + sizeV);
              std::fill(out.begin(), out.end(), 0);// clear object for reuse
              sizeV += out.size();
          }
        } //end p
        Rcpp::NumericVector out2(dim+1);
        out2 = esf_mst_sum_vector_s2(outvector, rcum.size(), rcum, eps_position, minSolved_ll, maxSolved_ll); // zweite Funktion anwenden
        for(int o = 0; o < out2.size(); o++){
          if(j < 1){
            out2[o] *=  eps_vec[v];
          } else {
            out2[o] *=  (eps_vec[v] * eps_vec[j-1]);
          }
       }
        // store vector in output
        if(j == 0){
          std::copy(out2.begin(), out2.end(), g_vec3.begin() + ((dim+1) * (dim) * (v)) + kk + 1);
        } else {
          std::copy(out2.begin(), out2.begin() + dim, g_vec3.begin() + (((dim + 1) * dim * v) + ((dim + 1) * (j-1))) + 1);
          std::copy(out2.begin(), out2.begin() + dim, g_vec3.begin() + (((dim + 1) * dim * (j-1)) + ((dim + 1) * (v)) + 1));
        }
        //helper index
        kk += out2.size()+1;

        std::fill(outvector.begin(), outvector.end(), 0);// clear object for reuse
        std::fill(out2.begin(), out2.end(), 0);// clear object for reuse
        std::fill(maxSolved_ll.begin(), maxSolved_ll.end(), 0);// clear object for reuse
        std::fill(minSolved_ll.begin(), minSolved_ll.end(), 0);// clear object for reuse
        std::fill(eps_position.begin(), eps_position.end(), 0);// clear object for reuse
        std::fill(rcum.begin(), rcum.end(), 0);// clear object for reuse
      } //end v
  }
    g_vec3.attr("dim") = dim_2o;
    esf_mst["2"] = g_vec3;
  }
 return(esf_mst);
}
