#' @export
print.renmr.state <- function(obj) {
    cat('renmr.state: ', obj$name, '\n')
    cat('taking types: ', paste(names(formals(environment(obj$new)$fun)), sep=', '), '\n')
}

# generic state factory
# should be used to build skeleton states
state.factory <- function(name, fun) {
    state <- list()
    class(state) <- 'renmr.state'
    state$name <- name
    state$new <- function(kind=NULL,...) {
#         res <- fun(self=as.list(state), kind=kind, ...)
        res <- fun(kind=kind, ...)
        res$name <- if(is.null(kind)) name else paste0(name,':',kind)
        res
    }
    
    # check state validity maybe?
    # 1) this would require us to half default-ful states
    #   i.e. can be built out of the box
    # 2) what does a state need to have?
    #   a) 'name' - name that incorporates kind
    #   b) 'init' - init
    #   c) 'f' - transition function
    test.state <- state$new(kind='foo')
    if(is.null(test.state$name)) stop('state has no name')
    if(test.state$name != paste0(name,':','foo')) stop(paste0('state [', name, '] fails to incorporate kind'))
    if(is.null(test.state$init)) warning(paste0('state [', name, '] has null initial state'))
    if(is.null(test.state$f)) stop(paste0('state [', name, '] has no transition function'))
    if(class(test.state$f) != 'function') warning(paste0('state [', name, '] transition function has unexpected class'))


    state
}

# RENMR CONTROL STATE

state.renmr.control <- state.factory(name='renmr.control',
                                     fun=function(properties=list(), ...) {
    res <- list(init=properties, f=function(state0, trans.df) state0)
})

# NULL STATE
state.empty <- state.factory(name='empty',
                             fun=function(...) {
    res <- list(init=0, f=function(state0, trans.df) 0)
})

# WEIGHTED NETWORK
halflife_decay <- function(data, halflife, dtime) {
    if(is.na(halflife) || dtime==0) return(data)

    return(data * exp(-dtime * (log(2) / halflife)))
}

state.wnetwork.matrix <- state.factory(name='renmr.wnetwork.matrix',
    fun=function(renmr.control=list(nnetwork=1),
                 src='__isrc__', trg='__itrg__', dtime='__dtime__', weight='weight',
                 halflife=NA, init=0, kind=NULL, type=kind, type.var='type', ...) {
        res <- list()

        n <- renmr.control$nnetwork
        res$init <- matrix(init,n,n)

        trans.f <- function(state.0, trans.df) {
            state.1 <- halflife_decay(state.0, halflife, trans.df[[dtime]])
            state.1[trans.df[[src]], trans.df[[trg]]] <- 
                state.1[trans.df[[src]], trans.df[[trg]]] + trans.df[[weight]]
            state.1
        }
        res$f <- trans.f

        if(!is.null(type)) {
            res$f <- function(state.0, trans.df) {
                if(trans.df[[type.var]] == type) {
                    trans.f(state.0, trans.df)
                } else {
                    state.0
                }
            }
        }

        res
    })

