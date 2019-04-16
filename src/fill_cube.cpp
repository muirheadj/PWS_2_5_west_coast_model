#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Cube<int> fill_cube_int(Cube<int> &Q, Mat<int> y, umat cube_idx) {
	// cube_idx is the combination of row/column/slice indices used to subset
	// the cube Q.
  uvec idx = sub2ind(size(Q), cube_idx.t());
  Q(idx) = y;
  return Q;
}


// [[Rcpp::export]]
cube fill_cube_dbl(cube &Q, mat y, umat cube_idx) {
	// cube_idx is the combination of row/column/slice indices used to subset
	// the cube Q.
  uvec idx = sub2ind(size(Q), cube_idx.t());
  Q(idx) = y;
  return Q;
}

// [[Rcpp::export]]
mat fill_mat_mat_dbl(mat &A, mat y, umat mat_idx) {
  // cube_idx is the combination of row/column/slice indices used to subset
  // the cube Q.
  uvec idx = sub2ind(size(A), mat_idx.t());
  A(idx) = y;
  return A;
}


// [[Rcpp::export]]
mat fill_mat_vec_dbl(mat &A, vec y, umat mat_idx) {
  // cube_idx is the combination of row/column/slice indices used to subset
  // the cube Q.
  uvec idx = sub2ind(size(A), mat_idx.t());
  A(idx) = y;
  return A;
}
