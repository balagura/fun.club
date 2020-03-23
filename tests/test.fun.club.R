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
## test transfer of ... and defining several links, letters[4:6] = 'd','e','f'
fc[letters[4:6], character.only=TRUE] <- function(...) {
    dots <- list(...)
    list(..1, ..2 * 2, length(dots))
}
## dependence on other function objects
fc[a] <- function(...) {
    dots <- list(...)
    list(d[...], e[...] * 2, f[...], dots)
}

test.adef <- function() {
    stopifnot(identical(f[3,4,5], 3L))
    arg1 <- c( 9, 19,   9,  19)
    arg2 <- c(10, 10, 110, 110)
    for (i in seq_along(arg1)) {
        x <- arg1[i]
        y <- arg2[i]
        stopifnot(identical(a[x,y], list(x, y*4, 2L, list(x, y))))
    }
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


fc[test.by.ref, by.reference = TRUE] = function(x, output.env) { output.env$test.by.ref = 2*x; x }
fc[test.by.ref1] = function(x) 2*x
stopifnot(test.by.ref1 [ 10 ] == test.by.ref [ 10 ])
fc[test.by.ref, by.reference = FALSE] = function(x, output.env) { output.env$test.by.ref = 2*x; x }
stopifnot(test.by.ref [ 10, new.env() ] == 10)

## test that x does not conflict with x argument in `[<-.fun.club`
fc[x]=function(x) 2*x
stopifnot(x[10] == 20)
## same for value, by.reference and character.only arguments in `[<-.fun.club`
fc[value]=function(x) x^2
stopifnot(value[10] == 100)
fc[by.reference]=function(x) x^2
stopifnot(by.reference[3] == 9)
fc[by.reference, by.reference=TRUE]=function(x, output.env) {output.env$by.reference = x^2; 100}
stopifnot(by.reference[3] == 9)
fc[character.only, character.only=FALSE]=function(x) x^2
stopifnot(character.only[3] == 9)
stopifnot( identical(fc[x], function(x) 2*x) )
stopifnot( identical(fc[value], function(x) x^2) )
stopifnot( identical(fc[by.reference], function(x, output.env) {output.env$by.reference = x^2; 100}) )
stopifnot( identical(fc[character.only], function(x) x^2) )

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

unload('fc')
unlink('test.fun.club', recursive=TRUE)
