library(testthat)

# testing states, stats
test.state <- state.factory(name='test.state', fun=function(kind=NULL, init=0, mult=1, ...) {
    res <- list()
    res$init <- init
    res$f <- function(state0, trans.df) { length(trans.df) + state0 * mult }
    res
})

test.stat1 <- stat.factory(name='stat.foo',
                           fun=function(x) { x[[ .(name(states[[1]])) ]] * x[[ .name(states[[2]]) ]] },
                           states=list(test.state(kind=kind1, init=(init=0)), test.state(kind=kind2)))
test.stat2 <- stat.factory(name='stat.bar',
                           fun=function(x) { x[[ .(name(states[[1]])) ]] },
                           states=list(test.state(kind=kind, init=(init=0))))


context('stat.factory')

# stat factory ---------------------------------------- 

test_that('stat.factory checks validity', {
    expect_error(stat.factory())

    # test improperly quoted function
    testfun <- function(x) {1}
    expect_error(stat.factory(name='', fun=testfun, state=list(), mapping=list()))

    # complains about lack of 'kind' in mapping
#     expect_warning(stat.factory(name='foo',
#                                 fun=function(x) {1},
#                                 states=list(state.renmr.wnetwork.matrix),
#                                 mapping=list(list(foo=bar))))
    expect_warning(stat.factory(name='foo',
                                fun=function(x) {1},
                                states=list(state.wnetwork.matrix(foo=bar))))
})

test_that('stat.factory preserves environment for constructor', {

})

test_that('stat.factory builds correct object', {
#     res.fcn <- stat.factory(name='foo',
#                             fun=function(x) { length( .(name(states[[1]])) ) },
#                             states=list(test.state, test.state),
#                             mapping=list(list(kind=kind1,n=quote(n=5)),
#                                          list(kind=kind2))
#                             )
#     res.fcn2 <- stat.factory(name='bar',
#                              fun=function(x) { length( .(name(states[[1]])) ) },
#                              states=list(test.state),
#                              mapping=list(list(kind=kind)))
    res.fcn <- stat.factory(name='foo',
                            fun=function(x) { length( .(name(states[[1]])) ) },
                            states=list(test.state(kind=kind1, n=(n=5)), test.state(kind=kind2)))
    res.fcn2 <- stat.factory(name='bar',
                             fun=function(x) { length( .(name(states[[1]])) ) },
                             states=list(test.state(kind=kind)))

    expect_true(is.function(res.fcn))

    expect_that(is.null(formals(res.fcn)$kind1), is_true())
    expect_that(is.null(formals(res.fcn)$kind2), is_true())
    expect_that(formals(res.fcn)$n, equals(5))

    res <- res.fcn(kind1='foo', kind2='bar')
    res2 <- res.fcn2(kind='foo')

    expect_that(name(res), equals('foo(foo,bar)'))
    expect_that(name(res$states[[1]]), equals('test.state:foo'))
    expect_that(name(res$states[[2]]), equals('test.state:bar'))
    expect_that(name(res2$states[[1]]), equals('test.state:foo'))
})



context('stat.factory:state extraction')

test_that('extract states works', {
#     stats <- list(test.stat1(kind1='foo'), test.stat2(kind='foo'), test.stat2(kind='bar'))
    stats <- list(test.stat1(kind1='foo', init=1), test.stat2(kind='foo', init=1), test.stat2(kind='bar'))

    res <- extract_states(stats)

    expect_that(length(res), equals(3))
    expect_that(name(res[[1]]), equals('test.state:foo'))
    expect_that(name(res[[2]]), equals('test.state'))
    expect_that(name(res[[3]]), equals('test.state:bar'))
})

test_that('initialize states works', {
#     test.env <- new.env()
#     test.env$name  <- name.renmr.state.requirement
#     test.env$name.renmr.state.requirement <- name.renmr.state.requirement

    stats <- list(test.stat1(kind1='foo', init=1), test.stat2(kind='foo', init=1), test.stat2(kind='bar'))
    state.reqs <- extract_states(stats)
#     res <- eval(initialize_states(state.reqs, additional.args=list(mult=2)), envir=test.env)
    res <- initialize_states(state.reqs, additional.args=list(mult=2))

    expect_that(res[['test.state:foo']]$init, equals(1))
    expect_that(res[['test.state']]$init, equals(0))
    expect_that(res[['test.state:bar']]$init, equals(0))

    init <- res$`test.state:foo`$init
    f <- res$`test.state:foo`$f

    expect_that(init, equals(1))
    expect_that(f(init, data.frame(a=1, b=1)), equals(4))
})

