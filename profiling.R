library(Rcpp)
library(digest)
library(profr)
library(proftools)
data(chat_network)

setwd('~/Dropbox/Code/renmr')

set.seed(1)
chat_small <- chat_network[which(runif(nrow(chat_network)) < 1.5),]
row.names(chat_small) <- NULL

# for(nm in list.files('src')) {
#     if(!exists('cpp.digests')) cpp.digests <- list()
#     nm.hash <- digest(file=paste0('src/', nm))
#     if(is.null(cpp.digests[[nm]]) || cpp.digests[[nm]] != nm.hash) {
#         cat(nm, '::' , cpp.digests[[nm]], '->', nm.hash, '\n')
#         cpp.digests[[nm]] <- nm.hash
#         rebuild <- TRUE
#     } else {
#         rebuild <- FALSE
#     }
#     sourceCpp(paste0('src/', nm), rebuild = rebuild)
# }
# for(nm in list.files('R')) source(paste0('R/',nm))

# start the reactor! free mars!
ptm <- proc.time()
# Rprof(filename='network.out', interval=0.01, line.profiling=TRUE, memory.profiling=TRUE)
res <- renmr.network(event.history=chat_small,
                     states=list(state.wnetwork.matrix(kind='friend'),
                                 state.wnetwork.matrix(kind='chat',halflife=10),
                                 state.wnetwork.matrix(kind='chatfast',type='chat',halflife=1)),
                     statistics=list(stat.constant(),
                                     stat.wnetwork.dyad('out','chat'),
                                     stat.wnetwork.dyad('in','chat'),
                                     stat.wnetwork.dyad('in','chatfast'),
                                     stat.wnetwork.dyad_product('out','out','chat','friend'),
                                     stat.wnetwork.dyad_product('in','out','chat','friend'),
                                     stat.wnetwork.dyad_product('in','out','chatfast','friend'),
                                     stat.wnetwork.degree('both','source','chat'),
                                     stat.wnetwork.degree('both','target','chat'),
                                     stat.wnetwork.triangle('both','both','chat','chat'),
                                     stat.wnetwork.triangle_product('both','both','out','chat','chat','friend')),
                     start.time=14336,
                     verbose=TRUE, print.level=2, iterlim=20)
print(summary(res))
# Rprof(NULL)
print(proc.time() - ptm)

# summaryRprof('network.out', lines='both', memory='both')
# plot(parse_rprof('network.out'))

