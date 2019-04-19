#include <cstring>
#include <iostream>
#include <cmath>
#include <random>
#include <ctime>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


//' Generates a stochastic list of population transition matrices for each
//' port.
//
//' @title Generates a stochastic list of population transition matrices for each
//'  port.
//' @param Aorig The baseline population transition matrix used as source to
//'  generate random deviates.
//' @param curtime The current time of the the model.
//' @param ds A data.frame storing the datetimes at which each port is
//'  freed from the effects of the time lags for each lifestage.
//' @return A list containing random deviates for the population transition
//'  matrix applied to ports.
// [[Rcpp::export]]
List port_stoch_pop(NumericMatrix Aorig, SEXP curtime, DataFrame ds){

  // Inputs
  //double r_sd = Rcpp::as<double>(asd); // Standard deviation
  time_t current_time = as<long>(curtime); // current time

  DataFrame D(ds); // pointer to data.frame containing time before reproduction

  // Processing
  vector<long> larval_time = D["larval_time"]; // time lag before larva finish developing
  vector<long> juvenile_time = D["juvenile_time"]; // time lag before juveniles can appear
  vector<long> maturity_time = D["mature_time"]; // time lag before maturity
  vector<long> reproductive_time = D["reprod_time"]; // time lag before reproducing


  // Derived indices
  //  int nm = Aorig.ncol() * Aorig.nrow(); // Number of elements in the population transition matrix
  int k = reproductive_time.size(); // Number of ports or ships (ie. rows in data.frame)

  // Temporary outputs

  // Outputs
  Rcpp::List poplist(reproductive_time.size()); // List to contain Aouts for multiple ports
  // random number generation
  std::random_device rd;
  std::mt19937 e2(rd());

  int l = 1000;

  // Generate deviates for the diagonals
  std::binomial_distribution <> A0_bd(l, Aorig[0]);
  std::binomial_distribution <> A5_bd(l, Aorig[5]);
  std::binomial_distribution <> A10_bd(l, Aorig[10]);
  std::binomial_distribution <> A15_bd(l, Aorig[15]);

  //off-diagonals
  std::normal_distribution <double> A1_nd(Aorig[1], Aorig[1]/10);
  std::binomial_distribution <> A6_bd(l, Aorig[6]);
  std::normal_distribution <double> A11_nd(Aorig[11], Aorig[1]/10);
  std::normal_distribution <double> A12_nd(Aorig[12], Aorig[12]/10);

  for(int j = 0; j < k; j++){ // for each ship or port
    NumericMatrix Aout(4,4);
    Aout.attr("dimnames") = clone(List(Aorig.attr("dimnames")));
    poplist[j] = Aout;

    Aout[0] = A0_bd(e2)/double(l);  // larval survival
    Aout[1] =  A1_nd(e2); // larva to cyprid transition
    Aout[2] = 0.00; // larva to juvenile transition
    Aout[3] = 0.00; // larva to adult transition
    Aout[4] = 0.00; //cyprid to larval transition
    Aout[5] = A5_bd(e2)/double(l);
    Aout[6] = 0.00;  // cyprid to juvenile transition is handled in another function
    Aout[7] = 0.00; //cyprid to adult transition
    Aout[8] = 0.00; //juvenile to larval transition
    Aout[9] = 0.00; // juvenile to cyprid transition
    Aout[10] = A10_bd(e2)/double(l); // juvenile survival
    Aout[11] = A11_nd(e2); // juvenile to adult transition
    Aout[12] = A12_nd(e2); // adult per-capita birth rate
    Aout[13] = 0.00; // adult to cyprid transition
    Aout[14] = 0.00; // adult to juvenile transition
    Aout[15] = A15_bd(e2)/double(l); // adult survival

    if (current_time < larval_time[j]) Aout[1] = 0.00; // time for larval development
    if (current_time < maturity_time[j]) Aout[11] = 0.00; // time to maturity
    if (current_time < reproductive_time[j]) Aout[12] = 0.00;  // time for adult reproduction

  } // end of j loop

  return poplist;
}

//' Generates a stochastic list of population transition matrices for each
//' ship.
//
//' @title Generates a stochastic list of population transition matrices for each
//'  ship.
//' @param Aorig The baseline population transition matrix used as source to
//'  generate random deviates.
//' @param curtime The current time of the the model.
//' @param ds A data.frame storing the datetimes at which each ship is
//'  freed from the effects of the time lags for each lifestage.
//' @return A list containing random deviates for the population transition
//'  matrix applied to ships.
// [[Rcpp::export]]
List ship_stoch_pop(NumericMatrix Aorig, SEXP curtime, DataFrame ds){

  // Inputs
  //double r_sd = Rcpp::as<double>(asd); // Standard deviation
  time_t current_time = as<long>(curtime); // current time

  DataFrame D(ds); // pointer to data.frame containing time before reproduction

  // Processing
  vector<long> larval_time = D["larval_time"]; // time lag before larva finish developing
  vector<long> juvenile_time = D["juvenile_time"]; // time lag before juveniles can appear
  vector<long> maturity_time = D["mature_time"]; // time lag before maturity
  vector<long> reproductive_time = D["reprod_time"]; // time lag before reproducing


  // Derived indices
  //  int nm = Aorig.ncol() * Aorig.nrow(); // Number of elements in the population transition matrix
  int k = reproductive_time.size(); // Number of ports or ships (ie. rows in data.frame)

  // Temporary outputs

  // Outputs
  Rcpp::List poplist(reproductive_time.size()); // List to contain Aouts for multiple ports
  // random number generation
  std::random_device rd;
  std::mt19937 e2(rd());

  int l = 1000;

  // Generate deviates for the diagonals
  std::binomial_distribution <> A0_bd(l, Aorig[0]);
  std::binomial_distribution <> A5_bd(l, Aorig[5]);
  std::binomial_distribution <> A10_bd(l, Aorig[10]);
  std::binomial_distribution <> A15_bd(l, Aorig[15]);

  //off-diagonals
  std::normal_distribution <double> A1_nd(Aorig[1], Aorig[1]/10);
  std::binomial_distribution <> A6_bd(l, Aorig[6]);
  std::normal_distribution <double> A11_nd(Aorig[11], Aorig[1]/10);
  std::normal_distribution <double> A12_nd(Aorig[12], Aorig[12]/10);

  for(int j = 0; j < k; j++){ // for each ship or port
    Rcpp::NumericMatrix Aout(4,4);
    Aout.attr("dimnames") = clone(List(Aorig.attr("dimnames")));
    poplist[j] = Aout;

    Aout[0] = A0_bd(e2)/double(l);  // larval survival
    Aout[1] =  A1_nd(e2); // larva to cyprid transition
    Aout[2] = 0.00; // larva to juvenile transition
    Aout[3] = 0.00; // larva to adult transition
    Aout[4] = 0.00; //cyprid to larval transition
    Aout[5] = A5_bd(e2)/double(l);
    Aout[6] = 0.00;  // cyprid to juvenile transition is handled in another function
    Aout[7] = 0.00; //cyprid to adult transition
    Aout[8] = 0.00; //juvenile to larval transition
    Aout[9] = 0.00; // juvenile to cyprid transition
    Aout[10] = A10_bd(e2)/double(l); // juvenile survival
    Aout[11] = A11_nd(e2); // juvenile to adult transition
    Aout[12] = A12_nd(e2); // adult per-capita birth rate
    Aout[13] = 0.00; // adult to cyprid transition
    Aout[14] = 0.00; // adult to juvenile transition
    Aout[15] = A15_bd(e2)/double(l); // adult survival

    if (current_time < larval_time[j]) Aout[1] = 0.00; // time for larval development
    if (current_time < maturity_time[j]) Aout[11] = 0.00; // time to maturity
    if (current_time < reproductive_time[j]) Aout[12] = 0.00;  // time for adult reproduction

  } // end of j loop

  return poplist;
}

