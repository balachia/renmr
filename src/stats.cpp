// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// Orient matrix orientations:
// 0 = outbound
// 1 = inbound
// 2 = symmetric (inbound + outbound)

arma::mat orient_mat(arma::mat matr, int dir) {
    switch(dir) {
        case 1: return trans(matr);
        case 2: return matr + trans(matr);
        default: return matr;
    }
}

NumericVector mat2type(arma::mat matr, bool noloops) {
    NumericVector out(matr.n_elem - (noloops * matr.n_rows));
    //Rcout << "mat2type size " << out.size() << std::endl;
    //return out;
    int n = matr.n_rows;
    int idx = 0;
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            if(noloops && i==j) continue;
            out[idx] = matr(i,j);
            idx++;
        }
    }
    return out;
}

// [[Rcpp::export(".wnetwork.dyad")]]
NumericVector wnetwork_dyad(NumericMatrix mat, int dir, bool noloops) {
    arma::mat matr(orient_mat(as<arma::mat>(mat), dir));

    return mat2type(matr, noloops);
}

// [[Rcpp::export(".wnetwork.dyad_product")]]
NumericVector wnetwork_dyad_product(NumericMatrix mat1, int dir1, NumericMatrix mat2, int dir2, bool noloops) {
    arma::mat matr1(orient_mat(as<arma::mat>(mat1), dir1));
    arma::mat matr2(orient_mat(as<arma::mat>(mat2), dir2));

    return mat2type((matr1 % matr2).eval(), noloops);
}

// [[Rcpp::export(".wnetwork.degree")]]
NumericVector wnetwork_degree(NumericMatrix mat, int dir, bool for_target, bool noloops) {
    arma::mat matr(orient_mat(as<arma::mat>(mat), dir));
    arma::vec degvec = arma::sum(matr, 1);

    int n = matr.n_rows;
    NumericVector out(n * (n - noloops));

    int idx = 0;
    for(int i = 0; i < n; i++) {
        //for(int j = 0; j < n - noloops; j++) {
        for(int j = 0; j < n; j++) {
            if(noloops && i == j) continue;
            if(for_target) {
                out[idx] = degvec(j);
            } else {
                out[idx] = degvec(i);
            }
            idx++;
        }
    }

    return out;
}

// [[Rcpp::export(".wnetwork.triangle")]]
NumericVector wnetwork_triangle(NumericMatrix mat1, int dir1, NumericMatrix mat2, int dir2, bool noloops) {
    arma::mat matr1(orient_mat(as<arma::mat>(mat1), dir1));
    arma::mat matr2(orient_mat(as<arma::mat>(mat2), dir2));

    return mat2type((matr1 * matr2).eval(), noloops);
}

// [[Rcpp::export(".wnetwork.triangle_product")]]
NumericVector wnetwork_triangle_product(NumericMatrix mat1, int dir1,
        NumericMatrix mat2, int dir2,
        NumericMatrix mat_prod, int dir_prod,
        bool noloops) {
    arma::mat matr1(orient_mat(as<arma::mat>(mat1), dir1));
    arma::mat matr2(orient_mat(as<arma::mat>(mat2), dir2));
    arma::mat matr_prod(orient_mat(as<arma::mat>(mat_prod), dir_prod));

    return mat2type(((matr1 * matr2) % matr_prod).eval(), noloops);
}

