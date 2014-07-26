library(Rcpp)

setwd('~/Dropbox/Code/renmr/')

source('R/renmr.R')
source('R/stat_factory.R')
source('R/states.R')
source('R/stats.R')

# sourceCpp('src/pois_nll.cpp', rebuild=TRUE, verbose=TRUE)
sourceCpp('src/pois_nll.cpp')

states.list <- lapply(1:10, function (x) list(renmr.control=list(nnetwork=4, ntype=12)))

.pois_nll(states_list=states.list,
          stats=list(stat.constant()$f, stat.constant()$f),
          event_times = rep(0.5,10),
          stats_params=c(1,2),
          types_active=c(1,2,3,4,5,NA,7,8,NA,10),
          keep_idx=c(1,2,4,6,7,8))

states.list <- list(list(renmr.control=list(ntype=1),a=1))
stats <- list(a=function(x) { x$a },
              b=function(x) { -x$a })
event.times <- 1
stats.params <- c(1,1)
types.active <- 1

.pois_nll(states_list=states.list,
          stats=stats,
          event_times=event.times,
          stats_params=stats.params,
          types_active=types.active,
          keep_idx=1)
