#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
cube cube_fill_row(cube &Q, mat y, umat cube_idx) {
	// cube_idx is the combination of row/column/slice indices used to subset
	// the cube Q.
  uvec idx = sub2ind(size(Q), cube_idx.t());
  Q(idx) = y;
  return Q;
}
