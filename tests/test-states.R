library(testthat)

context('state.factory')

# state factory ---------------------------------------- 

test_that('state.factory builds state factory', {
    res <- state.factory(name='foo', fun=function(...) {
        res <- list()
        res$init <- 0
        res$f <- function(s0, x) { s0 + x$weight }
        res
    })

    expect_that(res, is_a('renmr.state'))
    expect_that(res$name, equals('foo'))

    res.state <- res$new()

    expect_that(res.state$name, equals('foo'))
    expect_that(res.state$init, equals(0))
    expect_that(res.state$f(res.state$init, list(weight=1)), equals(1))

    res.state <- res$new(kind='bar')
    expect_that(res.state$name, equals('foo:bar'))
})

test_that('state.factory checks validity', {
    expect_that(state.factory('', function(...) list(f=function() {0})), gives_warning())
    expect_that(state.factory('', function(...) list(init=0)), throws_error())
    expect_that(state.factory('', function(...) list(init=0, f=1)), gives_warning())
})
