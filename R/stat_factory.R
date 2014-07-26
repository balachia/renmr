# what is a statistic?
# class 'renmr.statistic'
# list
#   'f' - function(states)
#   'states' - list[]
#       'state' - state constructor object (i.e. 'renmr.state')
#       'args' - arguments to the constructor
#           in particular, 'kind', the special argument

print.renmr.state.requirement <- function(obj) {
    cat('requirement:\n')
    print(obj$state)
    cat('defining: \n', paste0(names(obj$args), '=', obj$args, '\n'))
}


# name.stat.state <- function(states,n) {
#     if(is.null(states[[n]]$args$kind)) {
#         return(states[[n]]$state$name)
#     } else {
#         return(paste0(states[[n]]$state$name, ':', states[[n]]$args$kind))
#     }
# }

name.renmr.state.requirement <- function(obj) {
    if(is.null(obj$args$kind)) {
        return(obj$state$name)
    } else {
        return(paste0(obj$state$name, ':', obj$args$kind))
    }
}

#' Construct a statistic function
#'
#' 
stat.factory <- function(name, fun, states, extra.args=list(), name.args=list(), env=parent.frame()) {
# stat.factory <- function(name, fun, states, mapping) {
    # 'fun' - a function skeleton
    #   in particular, can include .() expressions which will be substituted by
    #       a list 'states' containing a list of instantiated states (e.g.
    #       augmented by kind and other attributes)
    # 'states' - list of 'renmr.state' objects, i.e. of state generators
    # 'mapping' - list (aligned in order to 'states') of 
    #   list of arguments to relevant state assigned to an expression that will
    #       build the state constructor
    # e.g. states=list(state1, state2)
    #   mapping=list([state1 args:] list(kind=kind1), [state2 args:] list(kind2='chat'))
    #   generates a statistic constructor: stat(kind1, kind2='chat')
    #   'fun' then is augmented by list(states=list(state1$new(kind1), state2$new(kind2)))

    # pull out inputs as expressions
#     sub.mapping <- substitute(mapping)
#     mapping <- if(is.name(sub.mapping)) mapping else sub.mapping

    sub.fun <- substitute(fun)
    if(is.name(sub.fun)) {
        if(!is.call(fun)) {
            stop(paste0("can't substitute into function for stat [", name, ']'))
        } else {
            fun <- fun
        }
    } else {
        fun <- sub.fun
    }

#     sub.def.states <- substitute(states)
#     def.states <- if(is.name(sub.def.states)) def.states else sub.def.states
    
    sub.states.mapping <- substitute(states)
    states.mapping <- if(is.name(sub.states.mapping)) states.mapping else sub.states.mapping
    
    sub.name.args <- substitute(name.args)
    name.args <- if(is.name(sub.name.args)) name.args else sub.name.args

    # build up the argument lists from user provided mapping
    # need to build up stat function
    # and mappings from stat function to relevant states
#     arg.lists <- parse_mapping(mapping=mapping, name=name)
    arg.lists <- parse_mapping(spec=states.mapping, name=name, env=env)
    
    # build stat constructor body
    # need to:
    #   1) load in states
    #   2) load in state arguments
    #   3) load in function skeleton, swapping in state names
    # NB there are some environment problems here, e.g. the inner bquote doesn't inherit package environment
    # this is bad...
    res.body <- bquote( {
        calling.env <- new.env(parent=parent.frame())

        def.states <- .(def.states)
        arg.lists <- .(arg.lists)
        fname <- .(name)
        name.args <- .(name.args)
        states <- lapply(1:length( def.states ), function(i) {
            res <- list(state= def.states[[i]])

            # for each state:
            #   use mapping provided in 'mapping' to map stat arguments to state arguments
            #   since arg lists from 'mapping' are provided as quotes, evalute them to get their value
            #   from function environment
            
            # jesus in christ fuckery:
            # why parent.frame(4)?
            # rise up 1 level to hit xfcn envir, 2 to hit lapply, 3 to hit ifcn, 4 to hit lapply
#             res$args <- lapply(arg.lists[[i]], function(x) eval(x, envir=parent.frame(4)))
            res$args <- lapply(arg.lists[[i]], function(x) {eval(x, envir=parent.env(environment()))} )
            class(res) <- 'renmr.state.requirement'
            res
        })

        calling.env$states <- states

        res <- list()
        class(res) <- 'renmr.statistic'
#         res$f <- eval(bquote(.(stat.f), list(states=states)))
        res$f <- eval(bquote(.(stat.f), calling.env))
        res$states <- states

        # incorporate extra name argsA
        if(length(name.args) > 0) fname <- paste(fname, unlist(name.args), sep=':', collapse=)
        # this *probably* works to set names
        kind.names <- unlist(lapply(arg.lists, function(x) as.character(eval(x$kind))))
        res$name <- if (length(kind.names) == 0) fname else paste0(fname,'(',paste(kind.names, collapse=','),')')

        res
    }, list(def.states=arg.lists$state.list,
            arg.lists=arg.lists$state.args,
            stat.f=fun,
            name=name,
            name.args=name.args))
#     }, list(def.states=def.states, arg.lists=arg.lists$state.args, stat.f=fun))

    # FINALLY, build the function
#     as.function(c(arg.lists$stat.arg.list, res.body), env=parent.frame())
    as.function(c(c(extra.args, arg.lists$stat.arg.list), res.body), env=parent.frame())
}

parse_mapping <- function(spec, name='', env=parent.frame()) {
    # alternative mapping format
    # pass states list to stat.factory as
    # list(state1(kind=kind1, statefoo=(statfoo=[default])), state2(kind=kind2))

    # generates annoying call tree:
    # ()
    #   list
    #   ()
    #       state1
    #       kind1
    #       ()
    #           (
    #           ()
    #               =
    #               statfoo
    #               [default]
    #   ()
    #       state2
    #       kind2
    
    # take state argument names from names(states[[2]]), names(states[[3]])

    # allow for empty states list?

    state.list <- list()
    arg.list <- list()
    state.args <- list()
    # sid = state id
    for(sid in 2:length(spec)) {
        # get state constructor object
        state.list[[sid-1]] <- eval(spec[[sid]][[1]], envir=env)

        state.args[[sid-1]] <- list()

        if(is.null(names(spec[[sid]])) || !('kind' %in% names(spec[[sid]]))) {
            warning(paste0('stat.factory for [', name, '] does not allow for kind in state ', sid-1))
        }

        # this should have at least one argument because of 'kind'
        # aid=arg id
        for(aid in 2:length(spec[[sid]])) {
            if(is.name(spec[[sid]][[aid]])) {
                al.addend <- list(NULL)
                names(al.addend) <- as.character(spec[[sid]][[aid]])

                sa.addend <- list(substitute(a, list(a=spec[[sid]][[aid]])))
                names(sa.addend) <- names(spec[[sid]])[aid]
            } else {
                # why [2][2], [2][3]?
                # cause have (`(), (`=, statarg, default))
                al.addend <- list(spec[[sid]][[aid]][[2]][[3]])
                names(al.addend) <- as.character(spec[[sid]][[aid]][[2]][[2]])

                sa.addend <- list(substitute(a, list( a=spec[[sid]][[aid]][[2]][[2]] )))
                names(sa.addend) <- names(spec[[sid]])[aid]
            }
            arg.list <- c(arg.list, al.addend)
            state.args[[sid-1]] <- c(state.args[[sid-1]], sa.addend)
        }
    }
    list(state.list=state.list, stat.arg.list=arg.list, state.args=state.args)
}

parse_mapping2 <- function(mapping, name='') {
    # TODO: streamline this dirty for loop shit

    # build up the argument list from the ridiculous entry format
    # without default: list(foo[state]=foo[stat])
    # with default: list(foo[state]= quote(foo[stat]=default[stat]))
    # output:
    # stat.arg.list = list(foo[stat]=default[stat], [...])
    #   meant as argument list for stat constructor function
    # state.args = list([state1:] list(foo[state]=foo[stat]), [state2:] list(...), ...)
    #   meant to add to argument list for state once state constructor is called
    arg.list <- list()
    state.args.list <- list()
    for (i in 2:length(mapping)) {
        state.args.list[[i-1]] <- list()

        if(is.null(names(mapping[[i]])) || !('kind' %in% names(mapping[[i]]))) {
            warning(paste0('stat.factory for [', name, '] does not allow for kind in state ', i-1))
        }
        
        # e.g. mapping=list(list(kind=kind1, foo=quote(foo1=1)))
        # kind1 is name; quote(foo1=1) is not a name, but has a 'names' attribute
        # jesus in actual christ this is fuckery
        for(j in 2:length(mapping[[i]])) {
            if( is.name(mapping[[i]][[j]]) ) {
                addend <- list(NULL)
                names(addend) <- as.character(mapping[[i]][[j]])

                sa.addend <- list(substitute(a, list(a=mapping[[i]][[j]])))
                names(sa.addend) <- names(mapping[[i]])[j]
            } else {
                addend <- list(mapping[[i]][[j]][[2]])
                names(addend) <- names(mapping[[i]][[j]])[2]

                sa.addend <- list(substitute(a, list( a=as.name(names(mapping[[i]][[j]])[2]) )))
                names(sa.addend) <- names(mapping[[i]])[j]
            }
            arg.list <- c(arg.list, addend)
            state.args.list[[i-1]] <- c(state.args.list[[i-1]], sa.addend)
        }
    }
    
    list(stat.arg.list=arg.list, state.args=state.args.list)
}

# mapping <- quote(list(list(kind=kind1), list(kind=kind2, art=quote(art1=1))))
# res2 <- stat.factory('', function(x) { 1 + x[[.(name.stat.state(states,1))]]},
#                      list(state.renmr.wnetwork.matrix, state.renmr.wnetwork.matrix),
#                      list(list(kind=kind1),list(kind=kind2, art=quote(art1=1))))
# 
# res2



