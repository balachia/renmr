library(data.table)

setwd('~/Dropbox/Code/renmr/prepwork')

dt <- fread('sample_data.txt')
dt[, V6 := NULL]

setnames(dt, names(dt), c('source', 'target', 'time', 'type', 'weight'))

dt <- dt[source != target]

chat_network <- as.data.frame(dt)
row.names(chat_network) <- NULL

save(chat_network, file='../data/chat_network.rda')

