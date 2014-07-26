library(testthat)

context('glue')

# state testing junk ---------------------------------------- 
n.time <- 10
n.type <- 4
n.stat <- 2

event.history <- data.frame(dtime=rep(1,n.time), state.var=1:n.time)
state.functions <- list( main=list(f=function(state,ev.state) {ev.state$state.var - state},
                                   name='main',
                                   init=0)
)
stats <- lapply(1:n.stat, function(x) { 
        force(x)
        function (state) { 
            base <- x + n.stat*state$main
            base:(base+n.type)
        }
    })
names(stats) <- letters[1:n.stat]

# tests ---------------------------------------- 

test_that('build_state unrolls to expected state', {
    res <- build_state(event.history, state.functions)

    expect_that(length(res), equals(n.time))
    expect_that(names(res[[1]]), equals('main'))
    expect_that(sapply(res, function (x) x$main), equals( rep(0:n.time,each=2)[1:n.time + 1] ))
})

test_that('em.base driver processes raw event history', {
#     print(event.history)

})

