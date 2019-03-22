#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace std;
using namespace arma;
//'@param population_transition A lefkovich matrix of population transition probabilities and per-capita reproduction
//'@param population_size A vector of population size at t-1.
//'@return Population size at time t
//'@export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
 arma::Mat<uword> popgrow_int(List pop_transition, mat pop_size){

  int nrow_lifestages = pop_size.n_rows, ncol_lifestages = pop_size.n_cols;

  arma::mat output_population(nrow_lifestages, ncol_lifestages);
  arma::Mat<uword> output(nrow_lifestages, ncol_lifestages);
  arma::mat r(4, 4);

  for (int i = 0; i < pop_transition.size(); i++) {
    mat r = pop_transition[i];
    output_population.col(i) = arma::ceil(r * pop_size.col(i));
  }
  output = conv_to< Mat<uword> >::from(output_population);
return output;
}

 // [[Rcpp::export]]
  arma::mat popgrow_dbl(List pop_transition, mat pop_size){

   int nrow_lifestages = pop_size.n_rows, ncol_lifestages = pop_size.n_cols;

   arma::mat output_population(nrow_lifestages, ncol_lifestages);
   arma::mat r(4, 4);

   for (int i = 0; i < pop_transition.size(); i++) {
     mat r = pop_transition[i];
     output_population.col(i) = arma::ceil(r * pop_size.col(i));
   }
 return output_population;
 }

