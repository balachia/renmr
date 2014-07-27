library(testthat)

# testing junk
# upper triangle
mat1 <- matrix(c(0,0,0,1,0,0,1,1,0),3,3)

# taxicab distance from diagonal
mat2 <- matrix(c(0,1,2,1,0,1,2,1,0),3,3)

# off diagonal = 1
mat3 <- matrix(c(0,1,0,1,0,1,0,1,0),3,3)

states.nsl <- list(renmr.control=list(self.loops=FALSE, nnetwork=3, ntype=6),
                   `renmr.wnetwork.matrix:foo`=mat1,
                   `renmr.wnetwork.matrix:bar`=mat2,
                   `renmr.wnetwork.matrix:baz`=mat3
                   )

context('stats')

test_that('stat.constant correct', {
    f <- stat.constant()$f
    res <- f(states.nsl)
    expect_that(res, equals(rep(1,6)))
})

test_that('stat.wnetwork.dyad correct', {
    f <- stat.wnetwork.dyad(dir='in', kind='foo')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(0,0,1,0,1,1)))

    f <- stat.wnetwork.dyad(dir='both', kind='foo')$f
    res <- f(states.nsl)
    expect_that(res, equals(rep(1,6)))
})

test_that('stat.wnetwork.dyad_product correct', {
    f <- stat.wnetwork.dyad_product(dir1='out',kind1='foo',dir2='in',kind2='bar')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(1,2,0,1,0,0)))

    f <- stat.wnetwork.dyad_product(dir1='out',kind1='foo',dir2='both',kind2='bar')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(2,4,0,2,0,0)))
})

test_that('stat.wnetwork.degree correct', {
    f <- stat.wnetwork.degree(dir='in',kind='foo')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(0,0,1,1,2,2)))

    f <- stat.wnetwork.degree(dir='both',kind='foo')$f
    res <- f(states.nsl)
    expect_that(res, equals(rep(2,6)))

    f <- stat.wnetwork.degree(dir='in',focus='target',kind='foo')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(1,2,0,2,0,1)))
})

test_that('stat.wnetwork.triangle correct', {
    f <- stat.wnetwork.triangle(dir1='out',kind1='foo',dir2='out',kind2='bar')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(1,1,2,0,0,0)))
})

test_that('stat.wnetwork.triangle_product correct', {
    f <- stat.wnetwork.triangle_product(dir1='out',kind1='foo',dir2='out',kind2='bar', dir.prod='out', kind.prod='baz')$f
    res <- f(states.nsl)
    expect_that(res, equals(c(1,0,2,0,0,0)))
})

