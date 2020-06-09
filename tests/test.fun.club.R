require(fun.club)

unlink('test.fun.club', recursive=TRUE)
library(fun.club)
extension.selector <- function(object) { c('rds') }
savers <- list(rds = c(function(object, file) saveRDS(object, file = file),
                       function(file) readRDS(file = file)))
restorer <- lapply(savers, function(x) x[-1])
## ---------- create from scratch ----------
fc <- make.fun.club(
    'test.fun.club',
    extension.selector = extension.selector,
    savers = savers
)
## test transfer of named, positional and ... arguments, and several links
fc['d','e','f','g', character.only=TRUE] <- function(x=1, y, ...) {
    print(list(...))
    dots <- list(...)
    list(..1, ..2 * 2, length(dots), x+y)
}
## dependence on other function objects
fc[a] <- function(z, w=10, ...) {
    dots <- list(...)
    print(w)
    print(list(...))
    list(z=z, w=w, dots=dots[ order(names(dots)) ], d=d[w=w, ...], e=e[...], f=f[...], g=g[...])
}

test.adef <- function() {
    stopifnot(identical(d[3,4,5,x=10], 4))
    stopifnot(identical(e[3,4,5,x=10], 10))
    stopifnot(identical(f[3,4,5,x=10], 2L))
    stopifnot(identical(g[3,4,5,x=10], 13))
    ##
    stopifnot(identical(a[1,2,3,4,5, z=10, x=20],
    ## in a: z=10, w=1, ... = 2,3,4,5,x=20
    ## in d[..., w=w]: x=20, y=2, ...=3,4,5,w=1
    ## in d[w=w, ...]: x=20, y=2, ...= w=1, 3,4,5 -> 1
    ## in efg[...]: x=20, y=2, ... = 3,4,5 -> 8, 3L, 22
                        list(z=10, w=1, dots=list(2,3,4,5,x=20),
                             d=3, e=8, f=3L, g=22)))
    stopifnot(identical(a[1,2,3,4,5, z=10, x=20],
                        a[ z=10, 1,  x=20, 2,3,4,5]))
}
test.adef()

test.io <- function(extension.selector, savers, restorer,
                    fc) {
    ## example of accessing internalfun.club data
    ## ls.str(environment(environment(fc$all.links)$restorer$fun))
    ##
    ## test extension.selector
    stopifnot(
        identical(environment(fc$all.links)$extension.selector,
                  extension.selector)
    )
    ## savers structure
    stopifnot(
        identical(environment(fc$all.links)$savers,
                  savers)
    )
    ## restorer
    stopifnot(
        identical(environment(environment(fc$all.links)$restorer$fun)$funs,
                  restorer)
    )
}
## test extension.selector, savers, restorer
test.io(extension.selector, savers, restorer, fc)

## ---------- test reloading ----------
extension.selector <- function(object) { c('rds', 'txt') }
savers <- list(rds = c(function(object, file) saveRDS(object, file = file),
                       function(file) readRDS(file = file)))
restorer <- lapply(savers, function(x) x[-1])
unload('fc')
fc <- make.fun.club(
    'test.fun.club',
    extension.selector = function(object) {
        c('rds', 'txt')
    },
    savers = list(rds = c(function(object, file) saveRDS(object, file = file),
                          function(file) readRDS(file = file)))
)
## test reloaded function objects
test.adef()
## test extension.selector, savers, restorer
test.io(extension.selector, savers, restorer, fc)

## ---------- 2nd reloading ----------
unload('fc')
fc <- make.fun.club('test.fun.club')

## test reloaded function objects
test.adef()
## test extension.selector, savers, restorer
## test.io(extension.selector, savers, restorer, fc)

## test redefining
fc['a'] = function(x) x
fc['b'] = function(x) a[1]+x
fc['cc'] = function(x) b[10]+x*2
stopifnot(identical(cc[10], 31))
stopifnot(identical(b[10], 11))
stopifnot(identical(a[1], 1))
stopifnot(identical(b[5], 6))
stopifnot(identical(a[100], 100))
b[[10]] <- NULL
stopifnot(identical(b[10], 11))
fc['b'] = function(x) a[2]+x
stopifnot(identical(b[10], 12))    
a[[2]] <- NULL
stopifnot(identical(b[20], 22))    
a[[2]] <- NULL
b[[10]] <- NULL
stopifnot(identical(cc[50], 112))    
stopifnot(identical(cc[100], 212))    


## test that fun.link can accept `*tmp*`, x and value arguments
fc[test] = function(`*tmp*` = 0, x = 0, value = 0) `*tmp*` * x * value
stopifnot(test[`*tmp*`=1, x=2, value=3] == 6)


fc[output.env.1] = function(x, output.env) { output.env $ output.env.1 = 2*x; x }
fc[output.env.2] = function(x) 2*x
stopifnot(output.env.1 [ 10 ] == output.env.2 [ 10 ])

## test that x does not conflict with x argument in `[<-.fun.club`
fc[x]=function(x) 2*x
stopifnot(x[10] == 20)

## same for value and character.only arguments in `[<-.fun.club`
fc[value]=function(x) x^2
stopifnot(value[10] == 100)
##
fc[character.only, character.only=FALSE]=function(x) x^2
stopifnot(character.only[3] == 9)

stopifnot( identical(fc[x], function(x) 2*x) )
stopifnot( identical(fc[value], function(x) x^2) )
stopifnot( identical(fc[character.only], function(x) x^2) )

## test file.ext
fc[a]=function(x, file.ext = 'txt') {
    print(file.ext)
    writeLines(as.character(x), con = file.ext)
    file.ext
}

a[1:10]
stored.content <- as.numeric(readLines(a[1:10]))
stopifnot(1:10 == stored.content)

## more complicated test of file.ext
fc[a,b]=function(x, file.ext = list(c('txt','gz'),'txt')) {
    print(file.ext)
    writeLines(as.character(x), con = file.ext[[ 1 ]][ 1 ])
    system(paste0('gzip -c ', file.ext[[ 1 ]][ 1 ], ' > ', file.ext[[ 1 ]][ 2 ]))
    writeLines(as.character(2*x), con = file.ext[[ 2 ]][ 1 ])
    file.ext # a list
}

a[1:10]
stored.content.1 <- as.numeric(readLines(a[1:10][ 1 ]))
stored.content.1.gz <- as.numeric(readLines(a[1:10][ 2 ]))
stored.content.2 <- as.numeric(readLines(b[1:10]))
stopifnot(1:10 == stored.content.1)
stopifnot(stored.content.1 == stored.content.1.gz)
stopifnot(stored.content.1 * 2 == stored.content.2)

## test of file.ext supplied by caller
fc[a,b]=function(x, file.ext) {
    print(file.ext)
    writeLines(as.character(x), con = file.ext[[ 1 ]][ 1 ])
    system(paste0('gzip -c ', file.ext[[ 1 ]][ 1 ], ' > ', file.ext[[ 1 ]][ 2 ]))
    writeLines(as.character(2*x), con = file.ext[[ 2 ]][ 1 ])
    file.ext # a list
}

a[1:10, file.ext = list(c('txt','gz'),'txt')]
stored.content.1 <- as.numeric(readLines(a[1:10, file.ext = list(c('txt','gz'),'txt')][ 1 ]))
stored.content.1.gz <- as.numeric(readLines(a[1:10, file.ext = list(c('txt','gz'),'txt')][ 2 ]))
stored.content.2 <- as.numeric(readLines(b[1:10, file.ext = list(c('txt','gz'),'txt')]))
stopifnot(1:10 == stored.content.1)
stopifnot(stored.content.1 == stored.content.1.gz)
stopifnot(stored.content.1 * 2 == stored.content.2)

## same after reload
fc[a,b]=function(x, file.ext = list(c('txt','gz'),'txt')) {
    print(file.ext)
    writeLines(as.character(x), con = file.ext[[ 1 ]][ 1 ])
    system(paste0('gzip -c ', file.ext[[ 1 ]][ 1 ], ' > ', file.ext[[ 1 ]][ 2 ]))
    writeLines(as.character(2*x), con = file.ext[[ 2 ]][ 1 ])
    file.ext # a list
}

unload('fc')
fc <- make.fun.club('test.fun.club')
a[1:10]
stored.content.1 <- as.numeric(readLines(a[1:10][ 1 ]))
stored.content.1.gz <- as.numeric(readLines(a[1:10][ 2 ]))
stored.content.2 <- as.numeric(readLines(b[1:10]))
stopifnot(1:10 == stored.content.1)
stopifnot(stored.content.1 == stored.content.1.gz)
stopifnot(stored.content.1 * 2 == stored.content.2)

unload('fc')
unlink('test.fun.club', recursive=TRUE)
fc <- make.fun.club(dir = 'test.fun.club')
fc[f1] = function(x) 2*x
fc[f2] = function(y=1, ...) f1[y] * sum(unlist(list(...)))
fc[f3] = function() { function(n) { rnorm(n) } }
fc[f4, f5, f6] = function(a, b) list(f1[a+b], f2[a,b], f3[])
fc[f7] = function(a, b) f4[a,b] + f5[a,b]
stopifnot(identical(f7[1,2], 10))
fc['f1'] = function(x) x^2 # deletes all dependencies
stopifnot(identical(f7[1,2], 11))
## check that giving the same arguments by default or explicitly produces the
## same object
fc['a'] <- function(x=1, y, ...) { environment() }
stopifnot(!identical(a[x=1, y=2], a[y=2, z = 3]))

unload('fc')
unlink('test.fun.club', recursive=TRUE)
