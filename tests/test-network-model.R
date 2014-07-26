library(testthat)

# skeleton junk
event.history <- data.frame(time=1:20, source=rep(1:4,5), target=rep(2:5,5))

context('network framework')

test_that('renmr.network solves', {
#     stats <- list(stat.constant(n=20))
    stats <- list(stat.constant())

    res <- renmr.network(event.history=event.history, statistics=stats, self.loops=FALSE)

#     print(res)
    expect_that(res$code, equals(1))
})

