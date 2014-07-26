library(Rcpp)
library(profr)
library(proftools)
data(chat_network)

setwd('~/Dropbox/Code/renmr')

set.seed(1)
chat_small <- chat_network[which(runif(nrow(chat_network)) < 0.5),]
row.names(chat_small) <- NULL

for(nm in list.files('src')) sourceCpp(paste0('src/', nm), rebuild = TRUE)
for(nm in list.files('R')) source(paste0('R/',nm))

# source('R/renmr.R')
# debugonce(renmr.network)
# debugonce(initialize_states)
# debugonce(em.base)
# debugonce(build_state)
# debugonce(halflife_decay)

ptm <- proc.time()
Rprof(filename='network.out', interval=0.01, line.profiling=TRUE, memory.profiling=TRUE)
# res <- renmr.network(event.history=chat_small,
#                      statistics=list(stat.constant()),
#                      start.time=14336,
#                      verbose=TRUE, print.level=2)
res <- renmr.network(event.history=chat_small,
                     states=list(state.wnetwork.matrix(kind='friend'),
                                 state.wnetwork.matrix(kind='chat',halflife=30)),
#                      statistics=list(stat.constant()),
                     statistics=list(stat.constant(),
                                     stat.wnetwork.relation('out','chat'),
                                     stat.wnetwork.relation('in','chat'),
                                     ),
                     start.time=14336,
                     verbose=TRUE, print.level=2, iterlim=1)
Rprof(NULL)
print(proc.time() - ptm)

# summaryRprof('network.out', lines='both', memory='both')
plot(parse_rprof('network.out'))
# proftools.res <- readProfileData("network.out")
# plotProfileCallGraph(proftools.res)

