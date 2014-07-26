library(hash)
library(rbenchmark)
library(microbenchmark)

n.time <- 1000
n.type <- 10^2
n.stat <- 5

stats.array <- array(rnorm(n.time * n.stat * n.stat), dim=c(n.time, n.type, n.stat))
event.times <- runif(n.time)
stats.params <- rnorm(n.stat) / 10
types.active <- array(runif(n.time * n.type) < 0.1, dim=c(n.time, n.type))
event.support <- array(runif(n.time * n.type) < 0.9, dim=c(n.time, n.type))
state.list <- lapply(1:n.time, function(x) list(main=1))
stats <- lapply(1:n.stat, function(x) { function (state) rnorm(n.type) })
names(stats) <- letters[1:n.stat]

i.t <- 1

# random benchmarks
if (FALSE) {
    access.f <- function(x) { stats.array[x,,] }

#     benchmark(lapply(1:n.time, function(x) {stats.array[x,,]}),
#               lapply(1:n.time, function(x) {access.f(x)}),
#               replications=1e2, order='relative')

    microbenchmark(lapply(1:n.time, function(x) {stats.array[x,,]}),
                   lapply(1:n.time, function(x) {access.f(x)}),
                   times=100)
}

if (FALSE) {
    stats.slice <- lapply(stats, function(f) f(states.list[[i.t]]))
    f1 <- function(x) do.call(cbind,stats.slice)
    f2 <- function(x) matrix(unlist(stats.slice), nrow=n.type)

    microbenchmark(f1, f2, times=1e2)
}

if (FALSE) {
    n <- 1e3
    m <- 100
    junk <- lapply(1:n, function(x) matrix(rnorm(m^2),nrow=m))

    init <- diag(m)

    for.f <- function() { acc <- init; for (i in 1:n) acc <- solve(junk[[i]]); acc }
    red.f <- function() Reduce(function(a,b) solve(b), junk, init=init)
#     sum.f <- function() sum(as.numeric(1:n))

#     microbenchmark(for.f, red.f, times=1e2)
    benchmark(for.f, red.f, replications=1e2)
}

if (FALSE) {
    test <- lapply(letters, function (x) rnorm(1e2))
    names(test) <- letters
    test <- as.data.frame(test)

    brk.f <- function() test[['w']]
    dol.f <- function() test$w
    brk2.f <- function() test[w]
    brk3.f <- function() test[,w]

    microbenchmark(brk.f, dol.f, brk2.f, brk3.f, times=1e5)
}

# hashes are slow
# list lookups are slow
# bracketed list lookups are a little faster
# preconstructed is best
# closures are okest
if (TRUE) {
    ref <- list(a=1, b=2, renmr.control=list(ntype=3))
    refh <- hash(a=1, b=2, renmr.control=list(ntype=3))
    pre.clos <- function(x,y) { x^y }
    prec <- function(x) { x^3 }
    clos <- function(x) { pre.clos(x,y=3) }
    lkup <- function(x) { x^(ref$renmr.control$ntype) }
    lkpb <- function(x) { x^(ref[['renmr.control']][['ntype']]) }
    lkph <- function(x) { x^(refh$renmr.control$ntype) }

    x <- sample(1:100)

    microbenchmark(prec(x), clos(x), lkup(x), lkpb(x), lkph(x), times=1e4)
}
