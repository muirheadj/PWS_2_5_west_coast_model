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
extern "C" SEXP popgrow(SEXP population_transition, SEXP population_size){

  Rcpp::List pop_transition(population_transition);
  Rcpp::NumericMatrix pop_size(population_size);
  int nrow_lifestages = pop_size.nrow(), ncol_lifestages = pop_size.ncol();

  arma::mat input_population(pop_size.begin(), nrow_lifestages, ncol_lifestages, false);
  arma::mat output_population(nrow_lifestages, ncol_lifestages);
  Rcpp::NumericMatrix r(4,4);

  for (int i = 0; i < pop_transition.size(); i++) {
    Rcpp::NumericMatrix r = pop_transition[i];
    arma::mat m(r.begin(), nrow_lifestages, nrow_lifestages, false);
    output_population.col(i) = m * input_population.col(i);
  }
return Rcpp::wrap(output_population);
}
