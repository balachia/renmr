# UTILITY ---------------------------------------- 

mat2type.base <- function(mat, n, self.loops=FALSE) {
    if(self.loops) return(c(t(mat)))
    return( c(t(mat))[-seq(1,n^2,n+1)] )
}

mat2type <- function(mat, states) {
    mat2type.base(mat=mat,
                  n=states[['renmr.control']][['nnetwork']],
                  self.loops=states[['renmr.control']][['self.loops']])
}

wrap.network.matrix <- function(stat, n, self.loops=FALSE, ...) {
    function(...) {
        res.matrix <- stat(...)
        mat2type.base(res.matrix, n, self.loops)
    }
}

orient_mat <- function(mat, dir='both') {
    if (dir=='in') return(t(mat))
    if (dir=='both') return(mat + t(mat))
    return(mat)
}

# STATISTICS ---------------------------------------- 
# direction enumeration
# 0 - 'out'
# 1 - 'in'
# 2 - 'both' (sum of in + out)
dir.enum <- c(0,1,2)
names(dir.enum) <- c('out','in','both')

# CONSTANT
stat.constant <- stat.factory(name='constant',
                              states=list(state.empty(kind=kind)),
                              fun=function(x) {
#     res <- rep(1, 2 * x[['renmr.control']]$ntype )
#     res[1:x[['renmr.control']]$ntype]
    rep(1, x[['renmr.control']]$ntype )
})

# WEIGHTED MATRIX NETWORKS

stat.wnetwork.dyad <- stat.factory(name='dyad',
                                       extra.args=list(dir='both'),
                                       name.args=list(dir),
                                       states=list(state.wnetwork.matrix(kind=kind)),
                                       fun=function(x) {
#     mat <- orient_mat(x[[ .(name(states[[1]])) ]], dir=dir)
#     mat2type(mat,x)
    .wnetwork.dyad(mat=x[[ .(name(states[[1]])) ]],
                       dir=dir.enum[dir],
                       noloops= !x[['renmr.control']][['self.loops']])
})

stat.wnetwork.dyad_product <- stat.factory(name='dyad_product',
                                               extra.args=list(dir1='both', dir2='both'),
                                               name.args=list(dir1,dir2),
                                               states=list(state.wnetwork.matrix(kind=kind1),
                                                           state.wnetwork.matrix(kind=kind2)),
                                               fun=function(x) {
#     mat1 <- orient_mat(x[[ .(name(states[[1]])) ]], dir1)
#     mat2 <- orient_mat(x[[ .(name(states[[2]])) ]], dir2)
# 
#     mat2type(mat1 * mat2, x)
    .wnetwork.dyad_product(mat1=x[[ .(name(states[[1]])) ]],
                       dir1=dir.enum[dir1],
                       mat2=x[[ .(name(states[[2]])) ]],
                       dir2=dir.enum[dir2],
                       noloops= !x[['renmr.control']][['self.loops']])
})

stat.wnetwork.degree <- stat.factory(name='degree',
                                     extra.args=list(dir='both',focus='source'),
                                     name.args=list(dir,focus),
                                     states=list(state.wnetwork.matrix(kind=kind)),
                                     fun=function(x) {
#     mat <- orient_mat(x[[ .(name(states[[1]])) ]], dir)
#     rep(rowSums(mat), each=x[['renmr.control']][['nnetwork']] - (!x[['renmr.control']][['self.loops']]))
    .wnetwork.degree(mat=x[[ .(name(states[[1]])) ]],
                     dir=dir.enum[dir],
                     for_target=(focus=='target'),
                     noloops= !x[['renmr.control']][['self.loops']])
})

stat.wnetwork.triangle <- stat.factory(name='triangle',
                                       extra.args=list(dir1='both', dir2='both'),
                                       name.args=list(dir1,dir2),
                                       states=list(state.wnetwork.matrix(kind=kind1),
                                                   state.wnetwork.matrix(kind=kind2)),
                                       fun=function(x) {
#     mat1 <- orient_mat(x[[ .(name(states[[1]])) ]], dir1)
#     mat2 <- orient_mat(x[[ .(name(states[[2]])) ]], dir2)
# 
#     mat2type(mat1 %*% mat2, x)
    .wnetwork.triangle(mat1=x[[ .(name(states[[1]])) ]],
                       dir1=dir.enum[dir1],
                       mat2=x[[ .(name(states[[2]])) ]],
                       dir2=dir.enum[dir2],
                       noloops= !x[['renmr.control']][['self.loops']])
})

stat.wnetwork.triangle_product <- stat.factory(name='triangle_product',
                                       extra.args=list(dir1='both', dir2='both', dir.prod='both'),
                                       name.args=list(dir1,dir2),
                                       states=list(state.wnetwork.matrix(kind=kind1),
                                                   state.wnetwork.matrix(kind=kind2),
                                                   state.wnetwork.matrix(kind=kind.prod)),
                                       fun=function(x) {
#     mat1 <- orient_mat(x[[ .(name(states[[1]])) ]], dir1)
#     mat2 <- orient_mat(x[[ .(name(states[[2]])) ]], dir2)
#     mat.prod <- orient_mat(x[[ .(name(states[[3]])) ]], dir.prod)
# 
#     mat2type((mat1 %*% mat2) * mat.prod, x)
    .wnetwork.triangle_product(mat1=x[[ .(name(states[[1]])) ]],
                               dir1=dir.enum[dir1],
                               mat2=x[[ .(name(states[[2]])) ]],
                               dir2=dir.enum[dir2],
                               mat_prod=x[[ .(name(states[[3]])) ]],
                               dir_prod=dir.enum[dir.prod],
                               noloops= !x[['renmr.control']][['self.loops']])
})



