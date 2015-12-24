#include <Rcpp.h>
#include <math.h> 
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector inarow(NumericVector x) {
  int counter = 1;
  int position = 0;
  int i = 1;
  NumericVector y(3 * x.length());
  for(; i < x.length(); i++){
    if(x[i] == x[i - 1]){
      counter++;
    }
    else{
      y[position] = counter;
      y[position + 1] = x[i - 1];
      position = position + 2 + floor(log10(counter));
      counter = 1;
    }
  }
  y[position] = counter;
  y[position + 1] = x[i - 1];
  NumericVector z(position + 2);
  for(i = 0; i < position + 2; i++)
    z[i] = y[i];
  return z;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
input = 3113322113
digits = as.numeric(strsplit(as.character(input), "", fixed = TRUE)[[1]])
for(i in 1:50){
  digits = inarow(digits)
}
length(digits)
*/
