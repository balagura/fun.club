# fun.club - a workflow manager.

fun.club controls the generation of R objects, their caching in memory and the
storage on disk. It automatically tracks the object dependencies, so that if
one object is invalidated eg. by modifying its generating function, it is
deleted together with all dependencies.  Later, when referenced by the user,
it is automatically regenerated always using the most recent generating
functions. This is done behind the scenes, but the interface is transparent
for the user as shown in the following minimal example:
```
## create `fun.club`: a factory to generate `fun.objects`, ie. special 
## functions equipped with the capabilities to track and to cache all
## generated objects.
##
fc <- make.fun.club(dir = 'my_fun_club_directory')
##
## create the first "function object" `f1`
##
fc[f1] = function(x) x
##
## which can generate other objects as
##
f1[100]
##
## all such generated objects are cached and their dependencies are
## automatically tracked:
##
fc[f1] = function(x) 2*x
##
## f1[100] is automatically deleted and can be regenerated on demand:
##
f1[100]
##
## More complicated function with variable number of arguments in `...`
##
fc[f2] = function(y=1, ...) f1[y] * sum(unlist(list(...)))
f2[10, 1, 2, 3]
##
## The functions without arguments are also allowed. The functions can
## return arbitrary R objects (eg. other functions):
##
fc[f3] = function() { function(n) { rnorm(n) } }
##
## The function can return saveral objects placed in a `list`: `f4` below
## will return `f1[a,b]`, `f5` - `f2[a,b]` and `f6` - `f3[]`. This is
## useful if eg. the calculation gives two `data.frames` as a result, but
## they should be stored separately. This can be desirable eg. if the
## sizes of two objects are significantly different: there will be no need
## to keep in memory or reread from a file the big object to access the
## small one.
##
fc[f4, f5, f6] = function(a, b) list(f1[a+b], f2[a,b], f3[])
f4[1,2]
##
## Calling `f4` automatically generates `f5` and `f6'.
## `f4` and `f5` can be used as separate functions:
##
fc[f7] = function(a, b) f4[a,b] + f5[a,b]                    
##
## The request to generate `f7` object triggers the generation of all other
## objects it depends on
##
f7[1,2]
##
## since this `f7[1,2]` depends on `f1` (through `f5-f2`), changing `f1`
## deletes it together with all other dependencies:
##
fc['f1'] = function(x) x^2
##
## regardless of whether the objects were generated or not, syntactically
## they are always referred to in the same way, so the user might operate
## with them as if they were always available:
##
f7[1,2] + f6[3,4]                                                  
```

The package does not impose any limitation on the function object names,
any R names can be used (note that all variable names are limited in R to
10000 bytes, however, and were to 256 bytes in versions of R before 2.13.0,
see ?name). Any arguments can be used: named, positional and `...`. The
equivalent argument combinations like `a=1, 2, c=3` and `c=3, 1, 2` for a
function `function(a=1, b=2, ..., c=3)` are recognized and a new object is
generated only for new arguments.

The functions are considered equivalent if they are `deparse()`d into the
same character string. This means, in particular, that the code outside the
functions is not checked, eg. if the function object calls another function
not in `fun.club`, and this function changes, the objects will not be
deleted.

One can have many `fun.club`s open at the same time if they all point to
different physical directories.

