library(testthat)
library(Rcpp)

context('likelihood')


test_that('likelihood gives right value', {
    states.list <- list(list(renmr.control=list(ntype=1), a=1))
    stats <- list(a=function(x) { x$a },
                  b=function(x) { -x$a })
    event.times <- 1
    stats.params <- c(1,1)
    types.active <- 1

    res <- pois.nll(states.list = states.list,
                    stats=stats,
                    event.times=event.times,
                    stats.params=stats.params,
                    types.active=types.active)

#     print(res)

    expect_that(res[1], equals(1))
    expect_that(attr(res, 'gradient'), equals(c(0,0)))
    expect_that(attr(res, 'hessian'), equals(matrix(c(1,-1,-1,1),2)))
})
