// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
//#include <google/profiler.h>

using namespace Rcpp;

// [[Rcpp::export(".pois_nll")]]
List pois_nll(ListOf<List> states_list, ListOf<Function> stats,
        NumericVector event_times, arma::vec stats_params,
        IntegerVector types_active, IntegerVector keep_idx) {
    //ProfilerStart("gperf.log");

    // pull in inputs...
    // let's trust some implementation details: guaranteed to have state renmr.control with type
    List renmr_control = as<List>(states_list[0]["renmr.control"]);
    int ntype = (as<NumericVector>(renmr_control["ntype"]))[0];
    int nstat = stats.size();

    double nll = 0;
    arma::rowvec grad(nstat, arma::fill::zeros);
    arma::mat hess(nstat,nstat, arma::fill::zeros);

    List state;
    double event_time;
    IntegerVector active_type;
    int active_type_idx;
    bool has_active;

    // let's initialize some useful things
    arma::mat stats_slice(ntype,nstat, arma::fill::zeros);
    arma::vec type_exps(ntype, arma::fill::zeros);

    // loop over state indices
    for(IntegerVector::iterator it = keep_idx.begin(); it != keep_idx.end(); it++) {
        // offset everything by 1 because R != C
        event_time = event_times[*it - 1];
        active_type = types_active[*it - 1];
        active_type_idx = active_type[0] - 1;
        has_active = !is_na(active_type)[0];

        state = states_list[*it - 1];
        //Rcout << *it << "," << event_time << "," << active_type << "," << has_active << "," << std::endl;

        // make stats slice array
        for(int i = 0; i < nstat; i++) {
            // need to figure out a way to have in place modification of the stats.slice matrix
            Function f = stats[i];
            stats_slice.col(i) = as<arma::vec>(as<NumericVector>(f(state)));
        }
        type_exps = event_time * exp(stats_slice * stats_params);
        
        // add log likelihood
        nll += sum(type_exps);
        
        // add gradient
        for(int i=0; i < nstat; i++) {
            grad(i) += sum(stats_slice.col(i) % type_exps);
            //Rcout << grad << std::endl;
        }

        // add hessian
        for(int i=0; i < ntype; i++) {
            hess += type_exps(i) * trans(stats_slice.row(i)) * stats_slice.row(i);
            //Rcout << hess << std::endl;
        }

        // account for active type
        if(has_active) {
            //Rcout << stats_slice.row(active_type_idx) * stats_params << std::endl;
            nll -= (stats_slice.row(active_type_idx) * stats_params).eval()(0);
            grad -= stats_slice.row(active_type_idx);
        }
    }

    //ProfilerStop();

    // result list
    List res;
    res = List::create( Rcpp::Named("nll") = wrap(nll),
                        Rcpp::Named("gradient") = wrap(grad),
                        Rcpp::Named("hessian") = wrap(hess));
    return res;
}

