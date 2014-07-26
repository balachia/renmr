# rm(list=ls())

# TESTING DEFAULTS

# set.seed(1)
# 
# n.time <- 20
# n.actor <- 5
# n.type <- n.actor^2
# n.stat <- 5
# iter.lim <- 5
# 
# stat.names <- letters[1:n.stat]
# 
# stats.array <- array(rnorm(n.time * n.type * n.stat), dim=c(n.time, n.type, n.stat))
# event.times <- runif(n.time)
# stats.params <- rnorm(n.stat) / 10
# names(stats.params) <- stat.names
# types.active <- sample(c(1:n.type,NA), n.time, replace=TRUE)
# event.support <- array(runif(n.time * n.type) < 0.9, dim=c(n.time, n.type))
# states.list <- lapply(1:n.time, function(x) list(main=x, bork=-x))
# 
# stats <- lapply(1:n.stat, function(x) { 
#         force(x)
#         function (state) { 
#             set.seed(x + n.stat*state$main)
#             rnorm(n.type) 
#         }
#     })
# names(stats) <- stat.names
# 
# # let's make an event history...
# event.history <- data.frame(dtime=runif(n.time), bork=1:n.time)
# 
# evh.base <- data.frame(time=1:n.time,
#                        source=sample(1:n.actor, n.time, replace=TRUE),
#                        target=sample(1:n.actor, n.time, replace=TRUE))
# 
# # let's make some state functions
# state.functions <- list(
#         main=list(f=function(state,ev.state) {ev.state$bork},
#                   init=0)
#     )
# 
# i.t <- 1

# CORE FUNCTIONS

em.base <- function(event.history, state.functions, stats, initial.params=numeric(length(stats)), event.support=TRUE, keep.idx=1:nrow(event.history), verbose=FALSE, ...) {
    # event.history: data.frame
    #   sorted by event order
    #   (__dtime__) : numeric, time difference from last event (or from start)
    #   (__type__) : integer, event type
    # state.functions: list (list (init, f))
    #   init : initial state
    #   f (a,b) : given prior state *a* and changes *b*, returns next state
    
    # TODO: allow precomputation of states, stats
    # TODO: allow flexible column names
    # TODO: implement event support

    if (verbose) cat('Building state history\n')
    states.list <- build_state(event.history, state.functions)
    event.times <- event.history$`__dtime__`
    types.active <- event.history$`__type__`
        
    em.f <- function(params, ...) {
        vals.nll <- pois.nll(states.list=states.list,
                             stats=stats,
                             event.times=event.times,
                             stats.params=params,
                             types.active=types.active,
                             event.support=event.support)
        res <- vals.nll$nll
        attr(res, "gradient") <- vals.nll$gradient
        attr(res, "hessian") <- vals.nll$hessian
        return(res)
    }

    assign('em.f', em.f, pos=globalenv())

    if (verbose) cat('Starting ML optimization\n')
#     nlm.res <- nlm(f=em.f, p=initial.params, hessian=TRUE, ...)
    nlm.res <- nlm(f=pois.nll, p=initial.params, hessian=TRUE, 
                   states.list=states.list,
                   stats=stats,
                   event.times=event.times,
                   types.active=types.active,
                   event.support=event.support,
                   ...)
    if (verbose) cat('Done!\n')

#     cat('nlm code:', nlm.res$code, '\n')
    nlm.res

    res <- nlm.res
    res$names <- names(stats)
    class(res) <- c("renm")
    res
}

#' Entry to point to relational event network model
#' 
#' @param event.history raw event history containing (at minimum) event times, event sources and event targets
#' @param statistics a _list_ of model statistics to include, in renmr.statistic format
#' @param self.loops does the network allow for self-loop events (e.g. i sends event to i)?
#' @param verbose report solver progress?
#' @param ... parameters passed on to event_history.network, and other low level methods
#' @export
renmr.network <- function(event.history, statistics, states=list(), self.loops=FALSE, verbose=FALSE, ...) {
    if (verbose) cat('Processing event history\n')
    evh.res <- event_history.network(evh.base=event.history, self.loops=self.loops, ...)
    event.history <- evh.res$event.history
    n <- evh.res$n

    # parse states
    sub.states <- substitute(states)
    states <- if(is.name(sub.states)) states else sub.states
    states <- parse_states(states)

#     print(states)
    
    renmr.control <- state.renmr.control$new(properties=list(ntype=n*(n-!self.loops),
                                                             nnetwork=n,
                                                             self.loops=self.loops))

    if (verbose) cat('Parsing states and statistics\n')
    # initialize states
    states.list <- extract_states(statistics=statistics, pre.states=c(list(renmr.control), states))
    states.list <- initialize_states(state.requirements=c(states,states.list),
                                     additional.args=list(renmr.control=renmr.control$init))
    states.list <- c(list(renmr.control), states.list)

    # extract stat functions
    stats <- lapply(statistics, function(x) x$f)
    names(stats) <- lapply(statistics, name)

    em.res <- em.base(event.history=event.history,
                      state.functions=states.list,
                      stats=stats,
                      keep.idx=evh.res$keep.idx,
                      verbose=verbose, ...)
    em.res
}

event_history.network <- function(evh.base,
        time='time', src='source', trg='target', active=TRUE, start.time=0, n=NULL, self.loops=FALSE, ...) {
    # evh.base: data.frame

    # sort thing by time
    evh.out <- evh.base[order(evh.base[[time]]),]
    data.min.time <- min(start.time, evh.out[[time]])

    # get number of actors in network, and build the actor name lookup table
    actor.uniq <- na.omit(unique( c(unique(evh.out[[src]]), unique(evh.out[[trg]])) ))
    actor.uniq <- structure(1:length(actor.uniq), names=actor.uniq)
    nuniq <- length(actor.uniq)
    if(is.null(n)) {
        n <- nuniq
    } else {
        if (n < nuniq) stop(paste0('requested ',n, ' actors in network, but observed ', nuniq))
    }

    # check for self loop violations
    if(!self.loops && any(evh.out[[src]] == evh.out[[trg]])) {
        stop('found self-loops when none expected')
    }

    # TODO: make sure these column names don't overwrite shit...
    evh.out$`__dtime__` <- evh.out[[time]] - c(data.min.time, evh.out[[time]][1:(nrow(evh.out)-1)])
    evh.out$`__isrc__` <- actor.uniq[ as.character(evh.out[[src]]) ]
    evh.out$`__itrg__` <- actor.uniq[ as.character(evh.out[[trg]]) ]
    evh.out$`__type__` <- dyad2type(evh.out$`__isrc__`, evh.out$`__itrg__`, n, self.loops)

    res <- list()
    res$event.history <- evh.out
    # find important times
    # i.e. those with dtime > 0 or an active event
    res$keep.idx <- which(evh.out$`__dtime__` > 0 | !is.na(evh.out$`__type__`))
    res$n <- n
    res
}

pois.nll <- function(stats.params,
                     states.list, stats, event.times,
                     types.active=NA, event.support=TRUE, keep.idx=1:length(states.list),
                     ...) {
    # states.list: list (list(obj)): time x state x ?
    # stats: list (fun): stat
    # event.times: numeric, time
    # stats.params: numeric, stat
    # types.active: integer, time
    # event.support: logical, time x type x stat

    by.time <- function(i.t) {
        stats.slice <- lapply(stats, function(f) f(states.list[[i.t]]))
        stats.slice <- do.call(cbind,stats.slice)
        type.exps <- event.times[i.t] * exp( apply(stats.slice, MARGIN=1, crossprod, stats.params) )

        hess.by.type <- lapply(seq_along(stats.slice[,1]), function(i.type) {
                               tcrossprod(stats.slice[i.type,]) * type.exps[i.type]
        })

        grad.by.type <- sweep(stats.slice, MARGIN=1,
                              type.exps,
                              '*')

        nll.by.type <- type.exps

        # factor in pdf at active event
        if(!is.na(types.active[i.t])) {
            grad.by.type[types.active[i.t],] <- grad.by.type[types.active[i.t],] - stats.slice[types.active[i.t],]
            nll.by.type[types.active[i.t]] <- nll.by.type[types.active[i.t]] - stats.slice[types.active[i.t],] %*% stats.params
        }

        res <- list()
        res$nll <- Reduce('+', nll.by.type)
        res$gradient <- apply(grad.by.type, MARGIN=2, sum)
        res$hessian <- Reduce('+', hess.by.type)
        res
    }

#     res.by.time <- lapply(seq_along(event.times), FUN=by.time)
#     res.by.time <- lapply(keep.idx, FUN=by.time)
# 
#     res <- list()
#     res$nll <- Reduce(function(a,b) a + b$nll, res.by.time, init=0)
#     res$gradient <- Reduce(function(a,b) a + b$gradient, res.by.time, init=0)
#     res$hessian <- Reduce(function(a,b) a + b$hessian, res.by.time, init=0)
#     res
    pre.res <- .pois_nll(states_list=states.list, stats=stats,
                     event_times=event.times, stats_params=stats.params,
                     types_active=types.active, keep_idx=keep.idx)
#     res$gradient <- as.numeric(res$gradient)
#     print(res) # BUG: purge this line
#     res

    res <- pre.res$nll
    attr(res,'gradient') <- as.numeric(pre.res$gradient)
    attr(res,'hessian') <- pre.res$hessian
    res
}

# STATE FUNCTIONS

#' Unroll the set of observed states
#'
#' This builds out the set of observed states, starting from all states' initial position and iteratively
#' building the next state
#'
#' @param event.history pre-processed event history
#'   (i.e. with all variables expected by state transition functions)
#' @param state.functions a list of state functions to use in the model
build_state <- function(event.history, state.functions) {
    states <- lapply(state.functions, function(x) x$init)
#     names(states) <- names(state.functions)
    names(states) <- vapply(state.functions, name, 'character')
    
    res <- list()

    # TODO: rewrite faster, possibly with lapply+assign
    for(i in 1:nrow(event.history)) {
        res[[i]] <- states

        states <- lapply(1:length(state.functions), function(j) state.functions[[j]]$f(states[[j]], event.history[i,]))
#         names(states) <- names(state.functions)
        names(states) <- vapply(state.functions, name, 'character')
    }
    
    res
}

parse_states <- function(spec, env=parent.frame()) {
    reqs <- list()

    sid.seq <- if (length(spec) < 2) NULL else 2:length(spec)
    for(sid in sid.seq) {
        # get state constructor
        reqs[[sid-1]] <- list()
        class(reqs[[sid-1]]) <- 'renmr.state.requirement'
        reqs[[sid-1]]$state <- eval(spec[[sid]][[1]], envir=env)
        reqs[[sid-1]]$args <- list()

        aid.seq <- if (length(spec[[sid]]) < 2) NULL else 2:length(spec[[sid]])
        for(aid in aid.seq) {
#             sa.addend <- list(spec[[sid]][[aid]])
            sa.addend <- list(eval(spec[[sid]][[aid]], envir=env))
            names(sa.addend) <- names(spec[[sid]])[aid]

            reqs[[sid-1]]$args <- c(reqs[[sid-1]]$args, sa.addend)
        }
    }
    reqs
}

# STATISTIC FUNCTIONS

#' Extracts unique set of requested states from list of statistics
#' 
#' Extracts the unique set of requested states for the event model from the list of statistics.
#'
#' @param statistics a list of renmr.statistic statistics
#' @param pre.states a list of user pre-specified states
extract_states <- function(statistics, pre.states=list()) {
    res <- list()
#     stat.names <- character(0)
    stat.names <- vapply(pre.states, name, 'character')
    offset <- length(stat.names)

    idx <- 1
    for(i in 1:length(statistics)) {
        for(j in 1:length(statistics[[i]]$states)) {
            if(!(name(statistics[[i]]$states[[j]]) %in% stat.names)) {
                stat.names[idx+offset] <- name(statistics[[i]]$states[[j]])
                res[[idx]] <- statistics[[i]]$states[[j]]
                idx <- idx + 1
            }
        }
    }
    res
}

#' Initializes a set of states
#'
#' Initializes a set of states with arguments given by statistics and additional parameters
#'
#' @param state.requirements a list of state requirements provided by statistics
#' @param additional.args additional arguments to be passed to any (all) states
initialize_states <- function(state.requirements, additional.args=list()) {
    state.names <- vapply(state.requirements, name, 'character')
    res <- lapply(state.requirements, function (x) do.call(x$state$new, c(x$args, additional.args)))
    names(res) <- state.names
    res
}

# OUTPUT FUNCTIONS

print.summary.renm <- function(object) {
    cat('Relational Event Network Model\n')
    cat(object$iterations, ' iterations (nlm exit code ', object$code, ')\n\n', sep='')

    printCoefmat(object$coefficients)

    cat('\n')
    cat('LL:',object$LL,'\n')
    cat('AIC:',object$AIC,'\n')
}

summary.renm <- function(object) {
    ests <- object$estimate
    ses <- sqrt(diag(solve(object$hessian)))
    zs <- ests / ses
    pvals <- 2*pnorm(abs(zs), lower.tail=FALSE)

    res <- list()
    res$coefficients <- cbind(ests, ses, zs, pvals)
    dimnames(res$coefficients) <- list(object$names,
                                       c('Estimate', 'SE', 'z value', 'P > |z|'))
    
    res$LL <- -object$minimum
    res$AIC <- 2*(length(object$estimate) + object$minimum)

    res$code <- object$code
    res$iterations <- object$iterations

    class(res) <- 'summary.renm'
    res
}

# UTILITY FUNCTIONS

#' Extracts object name
#'
#' @param obj the object of interest
name <- function(obj) {
    UseMethod('name')
}

name.default <- function(obj) {
    tryCatch(obj$name, error=function(e) stop('object has no name defined'))
}

#' Convert (source, target) dyad to event type id
#' 
#' Network utility function that converts a (source, target) pair to the corresponding event type id
#'  in the general event model.
#'
#' @param src event source id (1-indexed)
#' @param trg event target id (1-indexed)
#' @param n network size
#' @param loops does the network allow for self-loops?
dyad2type <- function(src, trg, n, loops=FALSE) {
    (src-1)*(n-(!loops)) + trg - (!loops) * (trg > src)
}

#' Convert event type id to (source, target) dyad
#' 
#' Network utility that converts an event type id in the general event model to the corresponding
#'  event sender/receiver pair in a network.
#'
#' @param type event type id
#' @param n size of the network
#' @param loops does the network allow for self-loops?
type2dyad <- function(type, n, loops=FALSE) {
    i <- (type - 1) %/% (n - !loops) + 1
    j <- (type - 1) %% (n - !loops) + 1
    j <- j + (!loops) * (j >= i)

    list(src=i, trg=j)
}
