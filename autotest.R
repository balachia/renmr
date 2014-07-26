library(testthat)
library(Rcpp)

rm(list=ls())

tryCatch(detach("package:colorout", unload=TRUE), error=function (e) {}, finally={})

setwd('~/Dropbox/Code/renmr/')

for(nm in list.files('src')) sourceCpp(paste0('src/', nm))

auto_test('R/','tests/', env=globalenv())

