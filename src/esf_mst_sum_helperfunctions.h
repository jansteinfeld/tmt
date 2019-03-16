#ifndef ESF_MST_SUM_HELPERFUNCTIONS_H
#define ESF_MST_SUM_HELPERFUNCTIONS_H
#include <Rcpp.h>
inline Rcpp::NumericVector combine(const List& list)
{
   std::size_t n = list.size();

   // Figure out the length of the output vector
   std::size_t total_length = 0;
   for (std::size_t i = 0; i < n; ++i)
      total_length += Rf_length(list[i]);

   // Allocate the vector
   NumericVector output = no_init(total_length);

   // Loop and fill
   std::size_t index = 0;
   for (std::size_t i = 0; i < n; ++i)
   {
      NumericVector el = list[i];
      std::copy(el.begin(), el.end(), output.begin() + index);

      // Update the index
      index += el.size();
   }

   return output;

}
#endif
