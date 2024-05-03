#include <Rcpp.h>
using namespace Rcpp;
#include "esf_mst_sum_vector_s1.h"
#include "esf_mst_sum_vector_s2.h"
#include "esf_mst_sum_helperfunctions.h"
// [[Rcpp::export]]
List esf_mst_sum_vector(List parlist, 
                        List ojlist, 
                        int order,
                        Rcpp::NumericVector minSolved, 
                        Rcpp::NumericVector maxSolved, 
                        Rcpp::NumericVector minSolved_design, 
                        Rcpp::NumericVector maxSolved_design,
                        Rcpp::NumericVector probs,
                        Rcpp::LogicalVector cumulative)
{
                            // )
  int psize = parlist.size();
  NumericVector size(psize+1);
  NumericVector sizeIndex(psize+1);

  List eps(psize);
  sizeIndex[0] = 0;
  for(int pp = 1; pp <= psize; pp++){
    NumericVector par = parlist[pp - 1];
    NumericVector eps_tmp(par.size());
    for(int ii = 0; ii < par.size(); ii++){
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
  if((order == 0) || (order == 1) || (order == 2)){

    int npar = size.size();
    int m = eps.size();
    
    NumericVector outvector(dim);
    NumericVector eps_position(npar);
    NumericVector oj(npar);
    NumericVector rcum(m);
    NumericVector ojlist_size = ojlist[0];
    
    rcum[0] = ojlist_size.size();
    eps_position[0] = 0;

    for(int p = 0; p < psize; p++){
      NumericVector par = eps[p];
      NumericVector oj = ojlist[p];
      Rcpp::NumericVector out = par.size();
      out = esf_mst_sum_vector_s1(par, oj); // erste Funktion anwenden
      outvector[Rcpp::Range( sizeIndex(p) + p, sizeIndex(p + 1) + p )] = out;
    }
    
    oj = combine(ojlist);
    
    for(int i = 1; i < m; i++) {
      NumericVector ojlist_i = ojlist[i-1];
      NumericVector ojlist_m = ojlist[i];
      rcum(i) = rcum(i-1) + ojlist_m.size();
      eps_position(i) = eps_position(i-1) + ojlist_i.size();
    }

    esf_mst["0"] = esf_mst_sum_vector_s2(outvector, m, rcum, eps_position, minSolved, maxSolved, minSolved_design, maxSolved_design, probs, cumulative); 
  }

  // --------------------------------------------------------------------------------------------------------------------
  /*
  * ----------------------------------------------
  * Start for first order here
  * ----------------------------------------------
  */
  if((order == 1) || (order == 2)){
    
    int m = eps.size();
    int k = 0; //helper index
    
    NumericMatrix esf_mat(dim + 1, dim);
    NumericVector size(psize);
    NumericVector sizeIndex(psize + 1);
    NumericVector minSolved_l = Rcpp::clone(minSolved);
    NumericVector maxSolved_l = Rcpp::clone(maxSolved);
    NumericVector minSolved_design_l(minSolved_design.size());
    NumericVector maxSolved_design_l(maxSolved_design.size());
    NumericVector probs_h(m);
    NumericVector rcum(m);
    NumericVector eps_position(m);
    NumericVector ojlist_size = ojlist[0];
    probs_h[0] = 0;
    rcum[0] = ojlist_size.size();
    eps_position[0] = 0;

    for(int i = 1; i < m; i++) {

      NumericVector ojlist_i = ojlist[i - 1];
      NumericVector ojlist_m = ojlist[i];
      rcum[i] = rcum[i - 1] + ojlist_m.size();
      if (cumulative[i]) {
        probs_h[i] = rcum[i - 1] + probs_h[i - 1] + 1;
      } else {
        probs_h[i] = rcum[i - 1] + i;
      }
      
      if(i < 2){
        eps_position(i) = eps_position(i-1) + ojlist_i.size() - 1;
      }else{
        eps_position(i) = eps_position(i-1) + ojlist_i.size();
      }
    }

    for(int l = 0; l < psize; l++){

      NumericVector outvector(dim-1);
      NumericVector probs_l = Rcpp::clone(probs);
      int helperase = 0;
      int index = 0;
      sizeIndex[0] = 0;

      for(int pp = 1; pp <= psize; pp++){
        NumericVector par = eps[pp - 1];
        size(pp - 1) = par.size();
        sizeIndex(pp) = sizeIndex(pp - 1) + size(pp - 1) - 1;
      }
      
      NumericVector eps_vec(sum(size));

      for(int v = 0; v < m; v++){
        NumericVector el = eps[v];
        std::copy(el.begin(), el.end(), eps_vec.begin() + index);
        index += el.size();
      }

      if(maxSolved[l] > 0) {
        maxSolved_l[l] = maxSolved[l]-1;
      }

      size[l] = size[l]-1;

      if(minSolved[l] > 0) {
         minSolved_l[l] = minSolved[l]-1;
      }

      if(l > 0){
        maxSolved_l[l-1] = maxSolved[l-1];

        if(minSolved[l-1] > 0){
          minSolved_l[l-1] = minSolved[l-1];
        }
        eps_position[l] = eps_position[l] + 1;
      }

      for(int ld = 0; ld < m; ld++){

        maxSolved_design_l[ld] = maxSolved_design[ld];
        minSolved_design_l[ld] = minSolved_design[ld];

        if(ld >= l){
          
          if ((maxSolved_design_l[ld] > 0) && (maxSolved_l[l] >= 0)) {
            maxSolved_design_l[ld] -= 1;
          }
           
          if((minSolved_design_l[ld] > 0) && (minSolved_l[l] >= 0)){        
            minSolved_design_l[ld] -= 1;
          }
        }
      }

      for(int i = l; i < m; i++){
        // ----------------------------------------------------------------------
        if (cumulative[i]) {
          probs_l.erase(probs_h[i]-helperase);
          helperase++;
        } else if (i==l){
          probs_l.erase(probs_h[l]);
          helperase++;
        }
      }
    
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
          out = esf_mst_sum_vector_s1(par, oj); 
          std::copy(out.begin(), out.end(), outvector.begin() + sizeV);
          std::fill(out.begin(), out.end(), 0);// clear object for re-use
          sizeV += out.size();
        }

        NumericVector out(dim);
        
        out = esf_mst_sum_vector_s2(outvector, m, rcum, eps_position, minSolved_l, maxSolved_l, minSolved_design_l, maxSolved_design_l, probs_l, cumulative);
        out.erase(dim);
        
        for(int r = 0; r < out.size(); r++){
          if (maxSolved[l] > 0) {
            esf_mat(r + 1, k) = out[r] *= eps_vec[k];
          } else {
            esf_mat(r + 1, k) = out[r] *= 0;
          }
        }
        k++;//helper index
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

    int m = eps.size();
    int k = 0.0;
    int kk = 0;
    
    NumericMatrix esf_mat(dim + 1, dim);
    NumericVector minSolved_l = Rcpp::clone(minSolved);
    NumericVector maxSolved_l = Rcpp::clone(maxSolved);
    NumericVector minSolved_design_l = Rcpp::clone(minSolved_design);
    NumericVector maxSolved_design_l = Rcpp::clone(maxSolved_design);
    NumericVector ojlist_i = ojlist[0];
    IntegerVector helper_stages_1(dim);
    NumericVector helper_stages_2(dim + 1);
    NumericVector eps_vec(dim);
    NumericVector g_vec3((dim + 1) * (dim) * (dim));
    // NumericVector size_i(size.size());
    IntegerVector dim_2o = IntegerVector::create((dim + 1),(dim),(dim));
    NumericVector probs_h(m + 1);
    NumericVector rcum(m);
    IntegerVector par_ii_position(dim);

    rcum[0] = ojlist_i.size();
    probs_h[0] = 0;
    helper_stages_2[0] = 0;

    for(int i = 0; i < m; i++) {
      NumericVector eps_i = eps[i];

      for(int ii = 0; ii < eps_i.size(); ii++){
        helper_stages_1[k] = i;
        par_ii_position[k] = ii;

        eps_vec[k] = eps_i[ii];
        k++;
      }
      if( i > 0 ){
        NumericVector ojlist_m = ojlist[i];
        rcum[i] = rcum[i - 1] + ojlist_m.size();
      
        if ( cumulative[i] ) {
          probs_h[i] = rcum[i - 1] + probs_h[i - 1] + 1;
        } else {
          probs_h[i] = rcum[i - 1] + i + 1;
        }    
      }
    }

    std::copy(helper_stages_1.begin(), helper_stages_1.end(), helper_stages_2.begin() + 1);
    
    if (Rcpp::any(cumulative).is_true()) {
      probs_h[m] = probs_h[m - 1] + dim + 1;
      probs_h = probs_h + 1;
    } else {
      probs_h[0] = 1;
    }
    
    for(int j = 0; j < helper_stages_1.size(); j++){
      for(int v = j; v < helper_stages_1.size(); v++){ //alle Bloecke in der Liste  
        NumericVector rcum(m);
        NumericVector eps_position(m);
        NumericVector minSolved_ll(m);
        NumericVector maxSolved_ll(m);
        NumericVector minSolved_design_ll(m);
        NumericVector maxSolved_design_ll(m);
        NumericVector outvector(dim - 1);
        NumericVector out2(dim+1);
        NumericVector probs_ll = Rcpp::clone(probs);
        NumericVector helperaseend(m);

        std::copy(maxSolved.begin(), maxSolved.end(), maxSolved_l.begin());
        std::copy(minSolved.begin(), minSolved.end(), minSolved_l.begin());
        std::copy(maxSolved_design.begin(), maxSolved_design.end(), maxSolved_design_l.begin());
        std::copy(minSolved_design.begin(), minSolved_design.end(), minSolved_design_l.begin());

        int helperase = 1;
        int helperaseendp = 0;
        int size_ii = 0;
        int sizeV = 0;
        int pp = 0;
        
        for(int i = helper_stages_2[j]; i < m; i++){
          
          if (cumulative[helper_stages_1[v]]) {

            if ((j==0) && (i >= helper_stages_1[v])) {
              probs_ll.erase(probs_h[i] - helperase);
              helperase++;
            } 

            if ((j!=0) && (i >= helper_stages_1[j-1])) {
              probs_ll.erase(probs_h[i] - helperase);
              helperase++;
            }

            if ((j!=0) && (i == helper_stages_1[v])) {
              probs_ll.erase(probs_h[i] + 1 - helperase);
              helperase++;
            } 

          } else if ((j!=0) && (helper_stages_1[j-1] == i)) {
              probs_ll.erase(probs_h[helper_stages_1[j-1]] - helperase);
              helperase++;
            } 

          if ((!cumulative[helper_stages_1[v]]) && helper_stages_1[v] == i && maxSolved[helper_stages_1[v]]!=0) {
            
            if (j==0) {
              probs_ll.erase(probs_h[i] - helperase);
            } else if (helper_stages_1[v] == helper_stages_1[j-1]) {
              probs_ll.erase(probs_h[i] + 1 - helperase);
            } else {
              probs_ll.erase(probs_h[i]  - helperase);
            }    
            helperase++;
          }  
          helperaseend[i] = helperase;
        }
        
        if ((j > 0) && cumulative[helper_stages_1[j]]){
        
          for(int i = (helper_stages_1[v] + 1); i < m; i++){
        
            if((helper_stages_1[v] != i) || (helper_stages_1[j] == helper_stages_1[v])) {
              probs_ll.erase(probs_h[i] - helperaseend[i-1] - helperaseendp);
              helperaseendp++;
            }
          }
        }
          
        for(int p = 0; p < psize; p++){
          
          NumericVector par_ii = eps[p];
          NumericVector oj_ii = ojlist[p];
          // int size_par_i = par_ii.size();
          int size_par_ii = par_ii.size();
          int helperase_par_ii = 0;

            if (helper_stages_1[v] == p) {
                
                par_ii.erase(par_ii_position[v] - helperase_par_ii);
                oj_ii.erase(par_ii_position[v] - helperase_par_ii);
                // Rcpp::Rcout << "a) par_ii: " << par_ii << std::endl;    
                helperase_par_ii++;
              } 

            if((j > 0) && (helper_stages_1[j-1] == p)){

                par_ii.erase(par_ii_position[j-1]);
                oj_ii.erase(par_ii_position[j-1]);
                helperase_par_ii++;
              } 
              
          if ((helper_stages_1[v] == p) || ((j > 0) && (helper_stages_1[j-1] == p))) {
            for (int i = 0; i < size_par_ii; i++) {

              if ((helper_stages_1[v] == p) && (v % size_par_ii == i)) {
                
                // order of conditions is important
                for (int lp = p; lp < psize; lp++){
                  // if ((maxSolved_l[helper_stages_1[v]] >= 0) || (cumulative[helper_stages_1[v]] && (maxSolved_design_l[lp]-1)>=0 )) {
                  if (((maxSolved_l[helper_stages_1[v]] >= 0) && (maxSolved_l[p] > 0)) || (cumulative[helper_stages_1[v]] && (maxSolved_design_l[lp]-1)>=0 )) {
                    maxSolved_design_l[lp] -= 1;
                  }
                  if ((minSolved_l[helper_stages_1[v]] > 0) || (cumulative[helper_stages_1[v]] && (minSolved_design_l[lp] -1)>=0 )) {
                    minSolved_design_l[lp] -= 1;
                  }
                }
                
                if (maxSolved_l[p] > 0) {
                  maxSolved_l[p] -= 1;
                }
                // size_i[p] -= 1;
                if (minSolved_l[p] > 0) {
                  minSolved_l[p] -= 1;
                } 
              } else if((j > 0) && (helper_stages_1[j-1] == p) && ((j-1) % size_par_ii == i)){

                // order of conditions is important
                 for (int lp = p; lp < psize; lp++){
                  // if ((maxSolved_l[helper_stages_1[j-1]] >= 0) || (cumulative[helper_stages_1[j-1]] && (maxSolved_design_l[lp]-1)>=0 )) {
                    if (((maxSolved_l[helper_stages_1[j-1]] >= 0) && (maxSolved_l[p] > 0)) || (cumulative[helper_stages_1[j-1]] && (maxSolved_design_l[lp]-1)>=0 )) {
                    maxSolved_design_l[lp] -= 1;
                  }
                  if ((minSolved_l[helper_stages_1[j-1]] > 0) || (cumulative[helper_stages_1[j-1]] && (minSolved_design_l[lp]-1)>=0 )) {
                    minSolved_design_l[lp] -= 1;
                  }
                }               
                
                if(maxSolved_l[p] > 0) {
                  maxSolved_l[p] -= 1;
                }
                // size_i[p] -= 1;
                if (minSolved_l[p] > 0) {
                  minSolved_l[p] -= 1;
                }
              } 
            }
          } 
            
            minSolved_ll[pp] = minSolved_l[p];
            maxSolved_ll[pp] = maxSolved_l[p];
            minSolved_design_ll[pp] = minSolved_design_l[p];
            maxSolved_design_ll[pp] = maxSolved_design_l[p];
            size_ii++;
           
            if(pp == 0){
              rcum[pp] = par_ii.size();
              eps_position[pp] = 0.0;
            }else{
              rcum[pp] = rcum[pp-1] + par_ii.size();
              eps_position[pp] = rcum[pp-1];
            }
            pp++;

          if(par_ii.size() != 0){
            Rcpp::NumericVector out = par_ii.size();
            out = esf_mst_sum_vector_s1(par_ii, oj_ii);
            std::copy(out.begin(), out.end(), outvector.begin() + sizeV);
            sizeV += out.size();
          } 
        } //end p

        out2 = esf_mst_sum_vector_s2(outvector, size_ii, rcum, eps_position, minSolved_ll, maxSolved_ll, minSolved_design_ll, maxSolved_design_ll, probs_ll, cumulative); 
        
        for(int o = 0; o < out2.size(); o++){
          
          if(j == 0){
            // out2[o] *= eps_vec[v];
            if ( maxSolved[helper_stages_1[v]] > 0 ) {
              // if ( (maxSolved[helper_stages_1[v]] > 0) && (maxSolved_ll[helper_stages_1[v]] != 0) ) {
              out2[o] *= eps_vec[v];
            } else {
              out2[o] *= 0;
            } 

          } else {    
            // if ((maxSolved[helper_stages_1[v]] != 0) && (maxSolved[helper_stages_1[j-1]] != 0)){
            if ( ((maxSolved[helper_stages_1[j-1]] > 0) && (maxSolved[helper_stages_1[v]] > 1)) || ((maxSolved[helper_stages_1[j-1]] > 1) && (maxSolved[helper_stages_1[v]] > 0))) {
              //  && (maxSolved_ll[helper_stages_1[v]] > 0)
              out2[o] *= (eps_vec[v] * eps_vec[j-1]);  
            } else {
              out2[o] *= 0;
            } 
            
          }          
        }
        
        if(j == 0){
          std::copy(out2.begin(), out2.end(), g_vec3.begin() + ((dim+1) * (dim) * (v)) + kk + 1);
        } else {
          std::copy(out2.begin()+1, out2.begin() + dim, g_vec3.begin() + (((dim + 1) * dim * v) + ((dim + 1) * (j-1))) + 2);
          std::copy(out2.begin()+1, out2.begin() + dim, g_vec3.begin() + (((dim + 1) * dim * (j-1)) + ((dim + 1) * (v)) + 2));
        }
        kk += out2.size()+1;
      } //end v
    }

    g_vec3.attr("dim") = dim_2o;
    esf_mst["2"] = g_vec3;
  }

 return(esf_mst);
}
