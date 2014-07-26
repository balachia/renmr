library(testthat)

context('network')

test_that('Network event history builder builds correctly', {
    evh.base <- data.frame(time=1:9, source=1:3, target=2:4)
    res.nsl <- event_history.network(evh.base, self.loops=FALSE)
    res.ysl <- event_history.network(evh.base, self.loops=TRUE)
    
    eh.nsl <- res.nsl$event.history
    eh.ysl <- res.ysl$event.history

    expect_that(eh.nsl$`__dtime__`, equals(rep(1,9)))
    expect_that(eh.ysl$`__dtime__`, equals(rep(1,9)))
    
    expect_that(eh.nsl$`__isrc__`, equals(rep(1:3,3)))
    expect_that(eh.nsl$`__itrg__`, equals(rep(2:4,3)))
    expect_that(eh.ysl$`__isrc__`, equals(rep(1:3,3)))
    expect_that(eh.ysl$`__itrg__`, equals(rep(2:4,3)))

    expect_that(eh.nsl$`__type__`, equals(rep(c(1,5,9),3)))
    expect_that(eh.ysl$`__type__`, equals(rep(c(2,7,12),3)))
})

test_that('renmr.wnetwork.matrix updates appropriately', {
    res.0 <- state.wnetwork.matrix$new(renmr.control=list(nnetwork=10), halflife=NA, init=1)
    state.0 <- res.0$init
    expect_that(state.0, equals(matrix(1,10,10)))

    state.0 <- res.0$f(state.0, list(`__isrc__`=1,`__itrg__`=1,`__dtime__`=1,weight=1))
    expect_that(state.0, equals(matrix(c(2, rep(1,99)), 10, 10)))

    res.1 <- state.wnetwork.matrix$new(renmr.control=list(nnetwork=10), halflife=1, init=1)
    state.1 <- res.1$init
    expect_that(state.1, equals(matrix(1,10,10)))

    state.1 <- res.1$f(state.1, list(`__isrc__`=1,`__itrg__`=1,`__dtime__`=1,weight=0))
    expect_that(state.1, equals(matrix(0.5,10,10)))

    state.1 <- res.1$f(state.1, list(`__isrc__`=1,`__itrg__`=1,`__dtime__`=1,weight=1))
    expect_that(state.1, equals(matrix(c(1.25, rep(0.25,99)), 10, 10)))
})

test_that('renmr.wnetwork.matrix uses "kind" appropriately', {
    res.0 <- state.wnetwork.matrix$new(renmr.control=list(nnetwork=10), kind='foo', init=1)
    state.0 <- res.0$init
    expect_that(res.0$name, equals('renmr.wnetwork.matrix:foo'))

    state.0 <- res.0$f(state.0, list(`__isrc__`=1,`__itrg__`=1,`__dtime__`=1,weight=1,type='bar'))
    expect_that(state.0, equals(matrix(1, 10, 10)))
    
    state.0 <- res.0$f(state.0, list(`__isrc__`=1,`__itrg__`=1,`__dtime__`=1,weight=1,type='foo'))
    expect_that(state.0, equals(matrix(c(2, rep(1,99)), 10, 10)))
})

test_that('wrap.network.matrix wraps stats correctly', {
    stat <- function() { matrix(1:9, 3) }

    res.T <- wrap.network.matrix(stat, 3, TRUE)
    res.F <- wrap.network.matrix(stat, 3, FALSE)

    expect_that(res.T(), equals(c(t(matrix(1:9,3)))))
    expect_that(res.F(), equals(c(4,7,2,8,3,6)))
})
