#include <RCppArmadillo.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
extern "C" SEXP fw_reduction_Cpp(SEXP Rship_pop, SEXP tt, SEXP Ps, SEXP pt, SEXP fwred){

  // position_array
  NumericVector vecArray(Ps);
  IntegerVector arrayDims = vecArray.attr("dim");
  const int n = arrayDims[0], m = arrayDims[1], l = arrayDims[2];
  List arrayDimnames = vecArray.attr("dimnames");
  cube posArray(vecArray.begin(), n, m, l, false);

  // time position in position_array (max 200)
  int cp = Rcpp::as<int>(pt);

  // full time slice
  int ct = Rcpp::as<int>(tt);

  // freshwater reduction
  double fw_red = Rcpp::as<double>(fwred);

  // ships_pop_array
  NumericVector ships(Rship_pop);
  IntegerVector shipDims = ships.attr("dim");
  int a = shipDims[0], b = shipDims[1], c = shipDims[2];
  List shipDimnames = shipDims.attr("dimnames");
  cube shipsArray(ships.begin(), a, b, c, false);

  //n = ships, m = ports, l = time
  //get the slice for tt - 1
  mat pos = posArray.slice(cp - 1); // get ships for Panama Canal
  // Get ships index that are at the canal
  uvec ships_idx = find(pos.col(1) > 0); //remember: array indexing starts at 0

  int n_ships = ships_idx.size();

  int nb = n_ships * b;
  // create an index of positions
  mat zmat(size(nb, 3), fill::zeros);

  // have them in vector form
  uvec zvec(nb);

  // replicate lifestages by number of ships
  uvec lifestages(4);
  std::iota(lifestages.begin(), lifestages.end(), 0);
  mat lifestage_mat(n_ships, b);

  for (int j=0;j <n_ships; j++)
    for(int i=0; i<b; i++)
      lifestage_mat(j, i) = lifestages(i);

  vec lifestages_repeated = vectorise(lifestage_mat, 0);

  // replicate ships by number of lifestages
  uvec shipvec_repeated = repmat(ships_idx, b, 1);

  //loop through zmat to create indices
  for (int i=0; i<nb; i++){
    zmat(i,0) = ct-2;
    zmat(i,1) = lifestages_repeated(i);
    zmat(i,2) = shipvec_repeated(i);
  }

  for (int k=0; k<nb; k++)
    shipsArray(zmat(k,0), zmat(k,1),zmat(k,2)) *= fw_red;

    return wrap(shipsArray);
}
