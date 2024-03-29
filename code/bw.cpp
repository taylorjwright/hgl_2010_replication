// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

# include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
List ComputeAlphaBeta(arma::vec y, arma::vec x, arma::mat WW,
                      arma::mat weight, arma::mat Z, arma::vec G) {
  
  int n = x.n_elem;
  
  arma::vec weight2 = weight.diag();
  arma::mat I;
  arma::mat M_W = I.eye(n, n) - WW * inv_sympd(WW.t() * weight * WW) * WW.t() * weight;
  arma::vec xx = M_W * x;
  arma::vec yy = M_W * y;
  arma::mat ZZ = M_W * Z;
  arma::vec Alpha = (diagmat(G) * Z.t() * weight * xx) / as_scalar(G.t() * Z.t() * weight * xx);
  arma::vec Beta = (Z.t() * weight * yy) / (Z.t() * weight * xx);
  arma::vec Gam = (Z.t() * weight * yy) / ((ZZ.t() % ZZ.t()) * weight2);
  arma::vec Pi = (ZZ.t() * weight * xx) / ((ZZ.t() % ZZ.t()) * weight2);
  
  return List::create(Alpha, Beta, Gam, Pi, G);
}