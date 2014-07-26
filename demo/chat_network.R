data(chat_network)

set.seed(1)
chat_small <- chat_network[which(runif(nrow(chat_network)) < 0.01),]
row.names(chat_small) <- NULL

ptm <- proc.time()
res <- renmr.network(event.history=chat_small,
                     statistics=list(stat.constant()),
                     start.time=14336,
                     verbose=TRUE, print.level=2)
print(proc.time() - ptm)
