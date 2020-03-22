#' @name fun.club
#'
#' @title fun.club: workflow manager
#' 
#' @description This is a workflow manager which controls the generation of R
#'     objects, their caching in memory and storing on disk. It automatically
#'     tracks the object dependencies, so that if one object is invalidated
#'     eg. by modifying its generating function, it is deleted together with
#'     all dependencies.  Later, when referenced by the user, it is
#'     automatically regenerated always using the most recent generating
#'     functions. This is done behind the scenes, but the interface is
#'     transparent for the user as shown in the following minimal example:
#'
#' ## create `fun.club`: a factory to generate `fun.objects`, ie. special 
#' ## functions equipped with the capabilities to track and to cache all
#' ## generated objects.
#' ##
#' fc <- make.fun.club(dir = 'my_fun_club_directory')
#' ##
#' ## create the first "function object" `f1`
#' ##
#' fc[f1] = function(x) x
#' ##
#' ## which can generate other objects as
#' ##
#' f1[100]
#' ##
#' ## all such generated objects are cached and their dependencies are
#' ## automatically tracked:
#' ##
#' fc[f1] = function(x) 2*x
#' ##
#' ## f1[100] is automatically deleted and can be regenerated on demand:
#' ##
#' f1[100]
#' ##
#' ## More complicated function with variable number of arguments in `...`
#' ##
#' fc[f2] = function(y=1, ...) f1[y] * sum(unlist(list(...)))
#' f2[10, 1, 2, 3]
#' ##
#' ## The functions without arguments are also allowed. The functions can
#' ## return arbitrary R objects (eg. other functions):
#' ##
#' fc[f3] = function() { function(n) { rnorm(n) } }
#' ##
#' ## The function can return saveral objects placed in a `list`: `f4` below
#' ## will return `f1[a,b]`, `f5` - `f2[a,b]` and `f6` - `f3[]`. This is
#' ## useful if eg. the calculation gives two `data.frames` as a result, but
#' ## they should be stored separately. This can be desirable eg. if the
#' ## sizes of two objects are significantly different: there will be no need
#' ## to keep in memory or reread from a file the big object to access the
#' ## small one.
#' ##
#' fc[f4, f5, f6] = function(a, b) list(f1[a+b], f2[a,b], f3[])
#' f4[1,2]
#' ##
#' ## Calling `f4` automatically generates `f5` and `f6'.
#' ## `f4` and `f5` can be used as separate functions:
#' ##
#' fc[f7] = function(a, b) f4[a,b] + f5[a,b]                    
#' ##
#' ## The request to generate `f7` object triggers the generation of all other
#' ## objects it depends on
#' ##
#' f7[1,2]
#' ##
#' ## since this `f7[1,2]` depends on `f1` (through `f5-f2`), changing `f1`
#' ## deletes it together with all other dependencies:
#' ##
#' fc['f1'] = function(x) x^2
#' ##
#' ## regardless of whether the objects were generated or not, syntactically
#' ## they are always referred to in the same way, so the user might operate
#' ## with them as if they were always available:
#' ##
#' f7[1,2] + f6[3,4]                                                  
#'
#' The package does not impose any limitation on the function object names,
#' any R names can be used (note that all variable names are limited in R to
#' 10000 bytes, however, and were to 256 bytes in versions of R before 2.13.0,
#' see ?name). Any arguments can be used: named, positional and `...`. The
#' equivalent argument combinations like `a=1, 2, c=3` and `c=3, 1, 2` for a
#' function `function(a=1, b=2, ..., c=3)` are recognized and a new object is
#' generated only for new arguments.
#'
#' The functions are considered equivalent if they are `deparse()`d into the
#' same character string. This means, in particular, that the code outside the
#' functions is not checked, eg. if the function object calls another function
#' not in `fun.club`, and this function changes, the objects will not be
#' deleted.
#'
#' One can have many `fun.club`s open at the same time if they all point to
#' different physical directories.
#' 
#' to do: technicalities:
#' 1) "normalization" of args, serialization and -> int
#' 2) fo names: All variable names are limited in R to 10000 bytes
#' (and were to 256 bytes in versions of R before 2.13.0, see
#' ?name). Therefore, as arguments one can use anything convertible with
#' as.character() to not more than 10000 bytes, and only that.
#' 3) separate envir's to keep names arbitrary
#' 4) escape hatch via char.only - rethink
#' 5) links can be destroyed and recreated - func for that?
#' 6) one can have several fc if all of them point to different directories
#' 7) *tmp* and value  in '['
#' 
#' @docType package
#' @name fun.club
#' @author Vladislav BALAGURA <balagura@cern.ch>
#'
#' @import Rcpp
#' @useDynLib fun.club
NULL  

## to do: wish list:
## 1) API to check that object is in memory
## 2) fc[ fo ] = NULL
## 3) fc[[ ]] - same as fc[] but with char.only
## Not used up to now:
##   fun.club[[]]
## to do: ?make.fun.club - saving args are unreadable; fst requires
## library(fst), can be dropped

#' Workflow manager
#'
#' This function creates the workflow manager of the class `fun.club` which
#' can be used to create fun.objects as described in `help('[<-.fun.club')`.
#' The workflow will be automatically saved in `file`. If `file` already
#' exists when the workflow is created, its content will be read to ...
#' to do: finish
#' 
#' @param dir The name of the directory where all generated objects are
#'     stored. If it exists, all object and `fun.club` itself will be read
#'     from there, otherwise, the directory will be created. The `fun.club`
#'     object is kept in the file "fun.club.dir" in this directory. The files
#'     with the objects have the names of the type "`dir`/ext/fun_n.ext", where
#'     ext is the file extension (eg. "rds"), "fun" is the name of the
#'     generating function and "n" is a positive integer.
#' 
#' @param envir environment where the `fun.club` and all its `fun.links` will
#'     be created.
#'
#' @param extension.selector The function receiving the `object` argument and
#'     returning the character vector with one or several file extensions
#'     corresponding to this object. The object will be saved to as many files
#'     as given in this vector and with the corresponding extensions. Eg. the
#'     object can be saved to the file with an "rds" extension as an R object
#'     and, additionally, to a "pdf" file as a plot. To retrieve the R object
#'     from the disk, only the file with the first extension will be used, so
#'     the extensions in the example above should be placed in the order "rds",
#'     "pdf".
#' 
#' @param savers The list containing read / write functions. Every list
#'     element is named by the corresponding file extenson and contains the
#'     list with either one or two functions. The first function receives the
#'     `object` and the `file` arguments and saves the object to this
#'     file. The second optional function receives only the `file`
#'     argument. It reads the object from this file and returns it. The
#'     reading function can be absent if the extension is not supposed to be
#'     used for retrieving the R object (eg. the "pdf" extension which is not
#'     suitable for that). In this case it never appears first in the
#'     character vector returned by the `extension.selector` function.
#'
#' @param verbose The integer which controls the amount of the printed
#'     diagnostic information, from 0 to 3 (default = 2). 0 means no output, 1
#'     - only the information on deleted or updated functions is printed, 2 -
#'     additionally, on the deleted and generated objects and on the
#'     operations with the files and the directories, 3 - additionally, on the
#'     stack content during the generation of the objects. All information is
#'     printed using `message()` function, so it can also be suppressed with
#'     `suppressMessages()`. `verbose` argument does not affect warnings and
#'     errors. The former can be suppressed using `suppressWarnings()`.
#' 
#' @return created fun.club object (invisibly)
#'
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' 
#' @export
#'
make.fun.club <- function(dir,
                          envir = globalenv(),
                          extension.selector = function(object) {
                              is.gg <- function(x) {
                                  any( c('ggplot','grob') %in% class(x))
                              }
                              if (is.gg( object ) ||
                                  is.list( object ) && all( lapply( object, is.gg ) == TRUE )) {
                                  c('rds', 'pdf')
                              } else if ('data.table' %in% class(object) &&
                                         ## for a moment `write_fst` can not
                                         ## store data.tables with the list as
                                         ## a column
                                         all( lapply( object,
                                                     is.atomic ) == TRUE ) &&
                                         ## `write_fst` can not store empty
                                         ## tables
                                         nrow( object ) > 0) {
                                  'fst'
                              } else if ('connection' %in% class(object)) {
                                  'raw'
                              } else {
                                  'rds'
                              }
                          },
                          savers =
                              ## c(fun1, fun2) returns *list* of fun1, fun2
                              list(rds = c(
                                       function(object, file)
                                           saveRDS(object, file = file),
                                       function(file)
                                           readRDS(file = file)
                                   ),
                                   fst = c(
                                       function(object, file)
                                           write.fst(object,
                                                     path = file,
                                                     compress = 100),
                                       function(file)
                                           read.fst(path = file,
                                                    as.data.table = TRUE)
                                   ),
                                   raw = c(
                                       function(object, file) {
                                           l <- length(object)
                                           writeBin(object = l, size = 8L)
                                           writeBin(object = object, con = file(file), open = 'wb')
                                       },
                                       function(file) {
                                           n.bytes <- readBin(con = file, what = integer(), n = 1L, size = 8L)
                                           readBin(con = file, what = raw(), n = n.bytes)
                                       }
                                   ),
                                   ## c(fun) returns *list* with fun as the 
                                   ## only element, list is required: fun
                                   ## instead of c(fun) would not work
                                   pdf = c(
                                       function(object, file) {
                                           pdf(file)
                                           if ('grob' %in% class(object)) {
                                               grid.newpage()
                                               grid.draw(object)
                                           } else {
                                               suppressWarnings(print(object))
                                           }
                                           dev.off()
                                       }
                                   )),
                          verbose = 2) {
    if (! file.exists( dir )) {
        if (! dir.create(dir, recursive = TRUE) ) {
            stop('Can not create directory ',dir)
        } else {
            dir <- normalizePath(dir)
            if (verbose >= 2) message(dir, ' is created')
        }
    } else  dir <- normalizePath(dir)
    ## after normalizePath(), `dir` contains absolute path
    new.exts <- character(0)
    for (ext in names(savers)) {
        subdir <- file.path(dir, ext)
        if (! file.exists( subdir )) {
            if (! dir.create(subdir, recursive = TRUE) ) {
                stop('Can not create subdirectory ',subdir)
            } else new.exts <- c(new.exts, ext)
        }
        ## clean environment which will be saved,
        ## subdir exists only if savers are not empty and looped
        ## over, so rm() it here
        rm(subdir)
    }
    if (verbose >= 2) {
        if (length(new.exts) > 0) {
            if (length(new.exts) == 1)
                message(file.path(dir, new.exts), ' is created for generated objects')
            else
                message(file.path(dir, paste0('{', paste0(new.exts, collapse=','), '}')),
                        ' are created for generated objects')
        }
    }
    file <- file.path(dir, 'fun.club.rds')
    ## to do: centralized clean 
    ## created after the loop but not needed afterwards. This environment will
    ## be saved, so delete `ext`, `new.exts`
    rm(ext, new.exts)
    ## this environment will be saved but with only relevant variables, in
    ## particular, `dir` and `file` will NOT be saved. They are used when
    ## `fun.club` is in memory, but deleted before saving it to disk.
    ##
    ## To read back `fun.club` object successfully, the argument to this
    ## function `dir` should point to the correct directory (where
    ## `dir`/fun.club.rds file resides). So, both for newly created `fun.club`
    ## and retrieved from disk, `dir` is taken from the arguments of this
    ## function and, therefore, never saved.
    ##
    ## This has the advantage that the WHOLE directory tree `dir` might be
    ## moved to another place between two R sessions. The only requirement is
    ## that the integrity of the tree is not broken ie. it is always moved as
    ## a whole. If `dir` argument is correct ie. points to the existing,
    ## potentially renamed, top of the tree, `fun.club` and all its
    ## constituents will be retrieved and later saved properly. There is no
    ## "anchor" linking `fun.club` to one particular place on the file system,
    ## it can freely slide together with its directory tree.
    ##
    ## On the other hand, other files in the same directories can be present
    ## and do not influence `fun.club` in any way as soon as their names do
    ## not conflict with the names of `fun.club` files.
    ##
    ## The internal structure of `fun.club` is always saved in
    ## `dir`/fun.club.rds file, while all other files contain saved values
    ## returned by executed `fun.club` functions. The latter are placed in the
    ## directories
    ## `dir`/name_of_the_function/number_of_arguments/1st_argument/2nd_arg/...
    ##
    ## All other arguments to this function can also be changed between the R
    ## sessions. Eg. `envir` is and "external" environment where the
    ## `fun.link`s are placed. Typically, it is the user's workspace
    ## `globalenv()` (this is the default). If not, eg. if `envir = e`, the
    ## function objects should be referenced as e$fun[arg1, ...], ie. with the
    ## prepended environment `e$` which might be less convenient. If `envir =
    ## globalenv()` the prefix `e$` is not needed. In this case, however,the
    ## user can freely perform in his workspace any operations, eg. by mistake
    ## he can remove the `fun.link`s or rename them to something
    ## else. Therefore, this environment is considered as "hostile" and
    ## `fun.link`s are on purpose made as "light" as possible and do not
    ## contain anything critical for `fun.club` operation. They can be
    ## recovered from internal `fun.club` environments. This also happens when
    ## `fun.club` is retrieved from disk: first, its internal environemnts are
    ## retrieved and then `fun.link`s are generated from them. If one changes
    ## `envir` between R sessions, new `fun.link`s will be created in a new
    ## place, but this will affect only the way how the user references them
    ## (the "prefix" `e$`), but not anything else.
    ##
    ## To do: describe saver's freedom
    ## to do: unload fc? fc = NULL if fc is active-binded?
    ## to do: clean environment
    ## to do: 1 -> 1L etc
    ## to do: explain arg encoder, with printable_arg
    ## to do: add saver?
    ## to do: add verbosity level
    ## to do: return methods
    if (file.exists( file )) {
        if (verbose >= 2) message('loading fun.club from ', file)
        ## to do: docs on save/restore environments and functions, on namespace saving
        ##
        e <- readRDS( file )
        if (verbose >= 2) message('in the end it will be saved to the same file')
        
        ## if `fun.club` is read from file, its operation environment is `e`,
        ## while if it is newly created, it is `environment()` of this
        ## function.  To ensure equal functionality in both cases, `e` and
        ## `environment()` should be populated equally.  `environment()`
        ## contains the arguments of this function and `file`, so, propagate
        ## them also to `e`:
        e $ dir   <- dir
        e $ file  <- file
        e $ envir <- envir
        e $ savers <- savers
        e $ extension.selector <- extension.selector
        e $ verbose <- verbose
        ## All `make.fun.club()` arguments can be changed freely between R
        ## sessions or after `unload(fun.club)`. They will be deleted by
        ## `e$save.on.exit()` just before saving everything else.
        for (fo in ls(all.names=TRUE, e$fun.env)) {
            e$create.fun.links( fo )
        }
        ## restore C++ arg encoder which associates function arguments to
        ## unique integers using saved to file list-of-lists containing
        ## strings and associated integers. This list-of-lists appears here as
        ## `e$arg.encoder` in the `e` environment. It is then used to populate
        ## the `new`ly created C++ encoder (`load_arg()` function below) which
        ## is returned to R under the same name `arg.encoder`. This object
        ## will coexist in memory with `fun.club`. When `fun.club` will be
        ## saved, C++ encoder will be agained `dump_arg()`ed to R
        ## list-of-lists. The C++ encoder will be deleted while the R
        ## list-of-lists will be saved to file under the same name
        ## `arg.encoder`.
        e$arg.encoder <- load_arg( e$arg.encoder )
        e$restorer $ reset.versions()
        for (i in seq_along( savers )) {
            s <- savers[[ i ]]
            ext <- names( savers )[ i ]
            if (length(s) == 2) e$restorer $ add( ext, s[[ 2 ]] )
        }
        reg.finalizer(e, onexit = TRUE, f = e$save.on.exit)
        e$clean.env( e )
        return( structure(class = 'fun.club', e$methods) )
    } else {
        if (verbose >= 2) message('`fun.club` will be saved to ', file)
        ## the arguments of this function automatically appear in this `environment()`
        fun.env  <- new.env( parent = emptyenv() )
        link.env <- new.env( parent = emptyenv() )
        ## create new C++ arg encoder which associates function arguments to
        ## unique integers
        arg.encoder <- new_arg_encoder()
        ## link.env will be populated with the character strings named by
        ## links and having values equal to the corresponding fun.object name.
        ## Ie. in case fun.club[res1,res2,res3] = ...
        ## eg.: link.env[['res3']] will give 'res1'
        ##
        ## "class" with "private" members:
        ##   funs - all restoring functions across sessions,
        ##          funs[[ extension ]] [[ version ]] == function
        ##   versions - keeps versions for *this session*, ie.
        ##              defined in make.fun.club(),
        ##              version [[ extension ]] == version
        make.restorer <- function() {
            funs <- list()
            versions <- list()
            reset.versions <- function() {
                versions <<- list()
            }
            add <- function(ext, f) {
                cat('before adding',ext,'\n')
                versions[[ ext ]] <<-
                    if (is.null( funs[[ ext ]] )) {
                        funs[[ ext ]] <<- list( f )
                        1
                    } else {
                        i <- anyDuplicated(c(f, funs[[ ext ]]))
                        if (i == 0) { # not  found
                            funs[[ ext ]] <<- c(funs[[ ext ]], f)
                            length(funs[[ ext ]])
                        } else {
                            i - 1
                            ## -1 not to count `f` in `c(f, funs[[ ext ]])`
                        }
                    }
                ## Comparison of functions can be done similarly to fun.objects:
                ## if (! isTRUE(all.equal(f.env $ fun, fun)) ||  # new fun
                ## 
            }
            version <- function(ext) versions[[ ext ]]
            fun <- function(ext, version) funs[[ ext ]] [[ version ]]
            list(reset.versions = reset.versions, add = add,
                 version = version, fun = fun)
        }
        restorer <- make.restorer()
        for (i in seq_along( savers )) {
            s <- savers[[ i ]]
            ext <- names( savers )[ i ]
            if (length(s) == 2) restorer $ add( ext, s[[ 2 ]] )
        }
        make.stack <- function() {
            ## s will be a list of lists of character vectors
            ## s[[1]] will be populated with parents after each function call like:
            ## s[[1]][[ i ]] = c(n.args, fo, Xs...)
            ##
            ## s[[2]], s[[3]], ... contain next recursion levels (as calculation
            ## of fun.objects can be nested: A can use B which uses C etc.)
            s <- list()
            len        <- function()  length( s )
            pop        <- function()  s <<- s[-1L]
            ## `x` argument should be a list of character vectors:
            push       <- function(x) s <<- c(list(x), s)
            ## unique is needed as one object can be used >1 times in
            ## generation
            ##
            ## It was surprisingly difficult to find unique vectors of
            ## character strings. In the end, I switched to just
            ## unique(list(such vectors)). From `?unique`: it works with
            ## vectors, data.frames and arrays (note, lists are not
            ## mentioned). On the other hand, there is a warning that for
            ## lists it can be slow. So, in fact, it works also for lists.
            ##
            ## Moreover, from `unique.data.frame` code one can see that it
            ## calls in turn `duplicated.data.frame`. If the data.frame has
            ## only one column which itself is a list, the code from the
            ## latter calls `.Internal(duplicated(x, other args))` where `x`
            ## is in fact a list. So, in this way `unique.data.frame` relies
            ## on the fact that `unique` can be applied for lists.
            ##
            ## Probably, current R lacks a good implementation of sorting a
            ## list (there is a function with the misleading `sort.list` name
            ## but in fact it is for vectors, not lists). Note also, that
            ## `unique` for `data.frame`s is not 100% reliable: it calls
            ## `duplicated.data.frame` while from `?duplicated.data.frame` and
            ## from its code one can see that it works
            ##
            ## "... by pasting together a character representation of the rows
            ## separated by ‘\r’, so may be imperfect if the data frame has
            ## characters with embedded carriage returns or columns which do
            ## not reliably map to characters."
            ##
            ## What happens is that the algorithm just `paste`s together all
            ## elements of the `data.frame` row with '\r' (CR) as a
            ## separator. This gives the character representation of every
            ## row. Then, it finds unique elements among these
            ## representations. As soon as `as.character()` conversion never
            ## produces '\r' for all `data.frame` fields, this should be
            ## safe. If not, one can construct the same representations from
            ## different rows, eg.: a\rb + \r + c == a + \r + b\rc. In this
            ## case, unique row will be dropped after `unique()`.  There is a
            ## warning about that in ?duplicated but no similar warning in
            ## ?unique.
            ##
            unique.top <- function() {
                if (length(s) > 0) unique( s[[ 1L ]] ) else character(0)
            }
            ## `x` argument should be a character vector:
            add.to.top <- function(x) s[[ 1L ]] <<- c(s[[ 1L ]], list(x))
            clear      <- function()  s <<- list()
            to.char    <- function()  {
                . <- sapply(seq_along(s),
                            function(i) {
                                paste0(i, ':(', obj.names(s[[ i ]]), ')')
                            })
                paste0(., collapse=' ')
            }
            list(len = len, pop = pop, push = push, unique.top = unique.top,
                 add.to.top = add.to.top, clear = clear, to.char = to.char)
        }
        stack <- make.stack()

        clean.env <- function(fun.club.env) {
            not.func <- Filter(function(n) !is.function(fun.club.env[[ n ]]),
                               ls(all.names = TRUE, fun.club.env ))
            keep <- c(
                ## pointer to C++ encoder. It converts a character
                ## representation of the function arguments (obtained with
                ## `serialize()`) to a positive integer. Together with the
                ## name of the function object it gives the unique
                ## identifier of every result returned by the function. If
                ## the result is discarded later, the integer number is
                ## "freed" and might be reused. The integers are kept as low
                ## as possible.  In addition, the encoder keeps the
                ## "printable" verson of the arguments in "human-readable"
                ## format (not necessarily unique).
                ##
                'arg.encoder'
                ## holds the functions necessary to restore objects from disk.
                ## Internally has list-of-lists `funs` such that `funs[[ ext
                ## ]] [[ version ]]` gives the saver function.  Here, `ext` is
                ## the file extension, and `version` distinguishes potentially
                ## different variants of `savers` defined in several calls to
                ## `make.fun.club` for the same `fun.club` object. In
                ## addition, it has a list `verions` containing `ext =
                ## function` pairs for the most recently defined savers in the
                ## last `make.fun.club` call. It is used to define versions
                ## for `fun.objects` created in this session.
                ##
                ## `restorer` has the following functions: `reset.versions(),
                ## add(), version(), fun()`
                ##
               ,'restorer'
                ## every time the object is generated by the `fun.club`
                ## function, recalled from the memory or retrieved from the
                ## disk, this is recorded in the `stack`. If during the
                ## generation of one object the call to the other is detected,
                ## the first object becomes the parent and the second - the
                ## child in the dependency tree. The `stack` can be
                ## arbitrarily deep (if one object calls the second, the
                ## second calls the third etc.) and wide (one object calls
                ## many). `stack` is implemented as an environment of the
                ## function containing list-of-lists-of pairs (function object
                ## name - unique identifier of the arguments). It also
                ## contains typical stack functions len(), pop(), push(),
                ## clear(), add.to.top(), unique.top(), to.char() which are
                ## only used to operate the `stack`.
               ,'stack'
                ## arguments given to `make.fun.club` function: `fun.club`
                ## stored everything in the directory `dir`. Objects are
                ## individually stored in the subdirectories `dir`/ext, where
                ## `ext` is the corresponding file name extension.
               ,'dir'
                ## all `fun.club` internal data (ie. everything except the
                ## objects themselves) is stored in this `file` when
                ## `fun.club` is removed from memory. The `file` name is
                ## always `dir`/fun.club.rds, where `fun.club.rds` is fixed
                ## and can not be changed. The user has a freedom only to
                ## choose the directory (`dir` argument of `make.fun.club`
                ## function). `fun.club` is saved to `file` either
                ## automatically in the end of the R session, or if the user
                ## explicitly calls `unload(fun.club)`. Later, it can be
                ## restored from `file` to its original state except the
                ## arguments `dir, envir, savers` and `extension.selector` can
                ## potentially be different. These variables are not saved but
                ## deleted just before saving.
               ,'file'
                ## `envir` is the environment where the `fun.club` object and
                ## all `fun.links` will be created. Typically, this is the
                ## global environment. If not, the user needs to create this
                ## environment first. In caase of the global environment, the
                ## user does not need to explicitly specify it in every call
                ## of the function object (eg. `f[...]` instead of
                ## `env$f[...]`). The disadvantage is that in the global
                ## environment it is easier to delete or rebind the `fun.link`
                ## or even `fun.club` by mistake. In the latter case
                ## `fun.club` is automatically saved to disk. If `fun.link` is
                ## accidentally deleted, it can be recreated by calling
                ## `fun.club[link.name]`. This will print the corresponding
                ## `link.name` function and recreate the link in `envir` if
                ## necessary. To make this possible, `fun.link` in 'envir` is
                ## actually only a link to the real function object.  The
                ## latter is kept in the "private" internal environment inside
                ## `fun.club`.
               ,'envir'
                ## `extension.selector` argument to `make.fun.club` is the
                ## function selecting the appropriate file extension(s). When
                ## given an object as an argument, it decides what should be
                ## the extension(s) of the file(s) where this object will be
                ## stored. All functions are saved, so no need to include
                ## 'extension.selector' in this list.
                ##
                ## `savers` argument defines two functions for reading to /
                ## writing from this file. It is a `list` of pairs (extension
                ## - input / output function pair).
               ,'savers'
                ##
                ## Level of verbosity in `message()`s
               ,'verbose'
                ##
                ## `fun.env` is the most important internal environment of
                ## `fun.club`. It contains the "real" function objects which
                ## include both the functions and the generated
                ## objects. `fun.env` is the root of a tree of environments:
                ## `fun.env[[ fo.name ]]` is an environment serving the
                ## function object with the name `fo.name`. `fun.env` does not
                ## contain any other objects (otherwise their names could be
                ## in conflict with arbitrary function object
                ## names).
                ##
                ## `fun.env[[ fo.name ]]` has:
                ##
                ## 1) `fun` - function,
                ##
                ## 2) `links` - a vector of link names which correspond to
                ## this function. There is one-to-many correspondence between
                ## the function and the links, because one function is allowed
                ## to return several results as a `list`, like in the
                ## following example: `fc[f1, f2] = function(a, b) list(a+b,
                ## a-b)`. In this case calling `f1[1,2]` causes the
                ## calculation of `f2[1,2]` and vice versa. The `fun.link`s
                ## all should have unique names. For simplicity, the name of
                ## the function object which corresponds to several links by
                ## definition is chosen to be the name of the first link (`f1`
                ## in this case). All link names are kept in this character
                ## vector `links`. When the function object is deleted, all
                ## its links should be deleted. To simplify this, the object
                ## `fun.env[[ fo.name ]]` represents the function object and
                ## not a link as a tree branch. Subdivision into the links
                ## will appear in the very end at the level of leaves,
                ## ie. generated objects.
                ##
                ## 3) `by.reference` - logical indicating whether the function
                ## just returns the result and it should be later saved to its
                ## final place, or the result will be saved to its final
                ## destination directly inside the function. If the result is
                ## a big object, the latter can be advantageous to avoid
                ## copying.
                ##
                ## Finally, `fun.env[[ fo.name ]]` contains environments
                ## corresponding to the given combination of arguments. The
                ## latter are uniquely mapped to the positive integers. The
                ## results returned by the generation function for the first
                ## combination of arguments are stored in
                ##
                ## 4) the environment called "1". Then, "2", "3" and so on,
                ## ie. the environments are named by the positive integer
                ## converted to the character string.
                ##
                ## fun.env[[ fo.name ]] [["1"]] and all the others "2", "3",
                ## ... contain the following:
                ##
                ## I) parents,
                ##
                ## II) children,
                ##
                ## each is a `list` of pairs (function object name - positive
                ## integer coding the arguments). Such pairs are unique
                ## identifiers of the generated object. `parents` code the
                ## objects which were used to generate the current
                ## object. Removing one of them will erase the current
                ## object. Removing the current object will erase all its
                ## `children`.
                ##
                ## III) data.env - environment with the generated objects,
                ## stored under the names of the corresponding links. Eg. for
                ## the example above, `fc[f1, f2] = function(a, b) list(a+b,
                ## a-b)` and if `f2[1,2]` corresponds eg. to the first
                ## combination of arguments: `1+2` will be saved to `fun.env $
                ## f1 [["1"]] $ data.env $ f1`, while `1-2` to `fun.env $ f1
                ## [["1"]] $ data.env $ f2`. If the user requests to remove
                ## the object corresponding eg. to `f2` from memory but to
                ## keep it on disk by typing f2[[1,2]] = NULL, the `f2` object
                ## in `data.env` environment is set to NULL. This also
                ## explains, why fun.env[[ fo.name ]] [["1"]] is an
                ## environment and not a `list`. In the latter case
                ## NULL'ifying one of the links would create a new list and
                ## require copying of all other, potentially big,
                ## objects. With environments the objects are modified "in
                ## place", so NULL'ifying `f2` does nothing with `f1`.
                ##
                ## IV) `file` - an unnamed `list` with 1, 2, ... elements
                ## corresponding to 1, 2, ... link of the given function (`f1`
                ## and `f2` above). Every element is a `list` of the form
                ## `list(ext = c('rds','pdf'), version = 1)`. It specifies the
                ## extension(s) of the file(s) generated for this object. The
                ## first is used to retrieve the object from the disk. Eg.
                ## one can store the plot object in *.rds file and the
                ## corresponding plot in *.pdf, then `ext = c('rds','pdf')`.
                ## For a given first extension `ext`, `restorer` keeps a
                ## list of versions of the functions able to retrieve the 
                ## object from this file. `version` determines which function 
                ## should be used.
               ,'fun.env'
                ## `link.env` contains only character strings. Every such
                ## object corresponds to one `fun.link` and has the same
                ## name. Since the names in one environment are unique, this
                ## ensures the uniqueness of `fun.link` names. The value of
                ## the character string is the name of the function object
                ## corresponding to this `fun.link`. Eg. in the example above,
                ## `fc[f1, f2] = function(a, b) list(a+b, a-b)`, there are
                ## `f1` and `f2` strings in `link.env` and `link[['f1']] ==
                ## link[['f2']] == 'f1', as the function object by convention
                ## is named after its first link `f1`.                
               ,'link.env'
                ## `link.methods` contain the subset of functions "exported"
                ## to `fun.link`s. More specifically,
                ##
                ## `fun.link` is
                ## 
                ## `structure(list(fun.object = function.object.name,
                ##                 i = sequential.number.of.the.link,
                ##                 link.methods = link.methods),
                ##            class='fun.link').
                ##
                ## In the example above: `f1` has `i=1` and `f2` has `i=2`.
               ,'link.methods'
                ## `methods` define a subset of "public" `fun.club` functions
                ## in C++ terminology, ie. directly visible from outside. In
                ## fact, `make.fun.club` function returns `methods`. If they
                ## are assigned to `fun.club` object, the methods can be
                ## called like `fun.club $ assign.fun.object(...)`. This is
                ## used to define the "overloaded" `fun.club` [] and []<-
                ## operators (`[.fun.club` and `[<-.fun.club`) outside of
                ## `make.fun.club` function.
               ,'methods'
            )
            delete <- setdiff(not.func, keep)
            rm(list = delete, envir = fun.club.env)
        }                                  
        
        ## fun.club internal variables and arguments to this function are
        ## propagated to the following functions through their enclosures
        ##
        ## Internal fun.club methods not exposed to ouside
        ##
        ## Functions producing character strings in `message()`s
        ##
        ## vector of character strings either of the form "{link1,link2,...}"
        ## or just "link". The first is to reference a common function serving
        ## several fun.links, the second is when the function has only one
        ## associated link.
        fun.names <- function(fos) {
            sapply(fos,
                   function(fo) {
                       links <- fun.env[[ fo ]] $ links
                       if (length(links) == 1L) {
                           links
                       } else paste0('{',
                                     paste0(links, collapse=','),
                                     '}')
                   }, USE.NAMES = FALSE)
        }
        
        ## `path` everywhere in the following is a character string with
        ##
        ## `(function name potentially common to several fun.links,
        ## serialized.argument.names,
        ## 1st argument value,
        ## 2nd argument value,
        ## ...).
        ##
        ## All function arguments can be absent, but the function name
        ## should always be present, therefore length(path) >= 1.
        ##
        ## Note that internally the object corresponding to `path`is stored in
        ##
        ## `fun.env[[ function name ]] [[ serialized.arg.names ]] [[
        ##   1st arg ]] [[ 2nd arg ]] ...`
        ##
        ## "serialized.arg.names" codes the sorted vector of all argument
        ## names.  For unnamed (positional) arguments it contains "".
        ##
        ## Specifically, "coded" means:
        ##
        ##   `rawToChar(serialize(names, connection=NULL, ascii=TRUE))`
        ##
        ## This form is chosen because it uniquely represents several
        ## character strings. Eg.
        ##
        ## `paste0(names, collapse='\n')`
        ##
        ## is not unique, as `names` can contain `\n` character, as in
        ## function(`a\nb`=1).
        ##
        ## There is a limitation in R that all variable names must have <=
        ## 10000 bytes. So, in principle, one can not "merge" in one variable
        ## eg. two arbitrary names each with the length of 10000 bytes. The
        ## function argument names are usually short, however, so typically
        ## this should not be a problem. Contrarily, the arguments themselves,
        ## represented via `as.character`, can be long, therefore each of them
        ## is allowed to have the maximal length of 10000 bytes, and each
        ## forms a separate hierarchy level.
        ##
        
        ## if `link` is NULL, returns character representation of `fo`, `ind`
        ## pair as "{link1,link2,...}[args]" or "link1[args]" if there is only
        ## one link. If `link` is not NULL, the representation is
        ## "link[args]". `fo` and `link` (if given) are strings, `ind` can be
        ## either a string or an integer.
        obj.name <- function(fo, ind, link = NULL) {
            arg <- printable_arg(arg.encoder,
                                 fo,
                                 as.integer(ind))
            if (is.null( link ))
                paste0(fun.names(fo), '[', arg, ']')
            else 
                paste0(link, '[', arg, ']')
        }

        ## Pastes several comma separated character representations of `fo`,
        ## `ind` pairs. Note, `link`s can not be given here. `fo.inds` is a
        ## list where each element is vector of character pairs `fo`, `ind`.
        obj.names <- function(fo.inds) {
            paste0( sapply( fo.inds, function(fo.ind) obj.name(fo.ind[ 1L ],
                                                               fo.ind[ 2L ])),
                   collapse = ', ')
        }
 
        ## links in "external" userspace environment can be deleted or
        ## rebinded to other objects by mistake
        ##
        ## returns a list with `ok`,`deleted` and `rebinded` vectors of the
        ## corresponding link integer indices in `fo $ links`
        link.status <- function(fo) {
            links <- fun.env[[ fo ]] $ links
            ok <- deleted <- rebinded <- integer(0)
            for (i in seq_along( links )) {
                link <- links[i]
                if (! is.null(x <- get0(link, envir = envir, inherits = FALSE))) {
                    if (class(x)          != 'fun.link' || # not correct link
                        x[['fun.object']] != fo         ||
                        x[['i']]          != i          ||
                        ! identical(x[['link.methods']], link.methods)) {
                        rebinded <- c(rebinded, i)
                    } else {
                        ok <- c(ok, i)
                    }                            
                } else deleted <- c(deleted, i)
            }
            list(ok =  ok, deleted = deleted, rebinded = rebinded)
        }

        ## returns "singular[1] name singular[2] rest" or "plural[1] name1,
        ## name2, ... plural[2] rest" depending on whether there is only one
        ## or several names in `names`
        complain <- function(names, singular, plural, rest) {
            l <- length(names)
            if (l > 0L) {
                s <- paste0(names, collapse=', ')
                if (l == 1L) {
                    s <- paste0(singular[1L],' ',s,' ',singular[2L])
                } else {
                    s <- paste0(plural[1L],' ',s,' ',plural[2L])
                }
                warning(s,' ', rest)
            }
        }

        ## check that all links of "fo" are neither accidentally deleted nor
        ## rebinded, otherwise, recreate them.
        check.fun.links <- function( fo ) {
            st <- link.status( fo )
            complain(st[['deleted']],
                     c('fun.object','has'), c('fun.objects','have'),
                     'been deleted and will be restored')
            complain(st[['rebinded']],
                     c('the name','has'), c('the names','have'),
                     paste0('been reassigned from fun.object to something else. ',
                            'It will be reassigned back'))
            for (i in c(st[['deleted']],
                        st[['rebinded']])) {
                missing.link <- fun.env[[ fo ]] $ links[[ i ]]
                envir[[ missing.link ]] <-
                    structure(list(fun.object = fo, i = i, link.methods = link.methods),
                              class='fun.link')
            }
        }

        ## Finds "fo" corresponding to "link", calls check.fun.links( fo ) 
        ## and returns fo's function
        check.fun.links.and.return.function <- function( link ) {
            fo <- link.env[[ link ]]
            check.fun.links( fo )
            fun.env[[ fo ]] $ fun
        }

        ## Creates `fun.link`(s) associated to a common function `fo`. If
        ## there is a name conflict ie. there is another object under the
        ## link's name, the  object is deleted with a warning
        create.fun.links <- function(fo) {
            links <-  fun.env[[ fo ]] $ links
            for (i in seq_along(links)) {
                link <- links[ i ]
                if (exists(link, envir = envir, inherits = FALSE)) {
                    rm(list = link, envir = envir) # name conflict
                    warning('The existed object ',link,' has been deleted')
                }
                envir[[ link ]] <-
                    structure(list(fun.object = fo, i = i, link.methods = link.methods),
                              class='fun.link')
            }
        }

        ## deletes all objects generated by common function `fo` together with
        ## their dependencies
        rm.all.generated <- function(fo) {
            all <- ls(all.names=TRUE, fun.env[[ fo ]])
            for (ind in grep('^[0-9]+$', all, value=TRUE)) {
                rm.generated(fo, ind)
            }
        }        

        ## deletes evrything associated to common functions specified by
        ## vector of their names `fos`:
        ##
        ## 1) links, with a `warning()` if any link is deleted or renamed,
        ## 2) generated objects together with their dependencies,
        ## 3) common functions themselves
        ##
        rm.fun.objects <- function(fos) {
            ## assumes all fun.objects in fos exist
            deleted <- rebinded <- character(0)
            for (fo in fos) {
                if (verbose >= 1) message('deleting ', fun.names( fo ))
                ## check before deletion whether all fun.link's in userspace
                ## are corresctly associated to fos
                links <- fun.env[[ fo ]] $ links
                ## get links before fun.env[[ fo ]] is deleted
                st <- link.status( fo )
                deleted  <- c(deleted,  links[ st[['deleted']] ])
                rebinded <- c(rebinded, links[ st[['rebinded']] ])
                rm(list = links[ st[['ok']] ], envir = envir) # delete correct links and also
                rm(list = links[ st[['rebinded']] ], envir = envir) # rebinded objects
                ## second, delete links in link.env 
                rm(list = links, envir = link.env)
                ## finally, delete fo
                rm.all.generated( fo )
                rm(list = fo, envir = fun.env)   
            }
            complain(deleted,  c('fun.object','has'), c('fun.objects','have'), 'been already deleted')
            complain(rebinded, c('the name','has'), c('the names','have'),
                     paste0('been reassigned from fun.object to something else. ',
                            'The reassigned object is DELETED'))
        }

        ## returns unique vector of common functions serving the given vector
        ## of links (without error checking)
        linked.funs <- function(links) {
            unique(
                sapply(links,
                       function( link ) link.env[[ link ]],
                       USE.NAMES = FALSE))
        }

        ## deletes everything associated to common functions serving given
        ## vector of links. A `warning()` is produced if the latter contains
        ## non-existing link names
        ##
        rm.links <- function(links) {
            ## links can be wrong
            wrong.names <- setdiff(links, ls(all.names=TRUE, link.env))
            l <- length(wrong.names)
            if (l > 0) {
                warning(paste0(wrong.names, collapse=', '),
                        if (l==1L) ' does' else ' do',
                        ' not exist and will not be deleted')
            }
            rm.fun.objects( linked.funs( links ))
        }

        ## initializes generation of objects by `fun` and referenced by
        ## `links`.  Nothing is done if the same function object already
        ## exists. If its common function differs from `fun`, it is
        ## updated. If `links` are associated to `fun` in a new way, ALL
        ## function objects having at least one conflicting link in common are
        ## deleted with `warning()`s.
        assign.fun.object <- function(links, fun, by.reference) {
            if (length( links ) > length( unique(links) ))
                stop('Link names must be unique')
            fos <- linked.funs( intersect( links, ls(all.names=TRUE, link.env) ))
            fos.print <- fun.names( fos )
            ## check whether one just needs to update fun
            if (length(fos) == 1L) {
                f.env <- fun.env[[ fos ]]
                if (identical(f.env $ links, links)) {
                    ## exact correspondance to one fun.object
                    if (! isTRUE(all.equal(f.env $ fun, fun)) ||  # new fun
                        f.env $ by.reference != by.reference ) {
                        rm.all.generated( fos )
                        if (verbose >= 1) message('updating ', fos.print)
                        f.env $ fun <- fun
                        f.env $ by.reference <- by.reference
                        ## assign in explicit environment
                        ## links match exactly, no need to relink
                    } else { ## check that fos's links are not accidentally
                        ## deleted in "external" environment, recreate
                        ## missing links if necessary
                        check.fun.links(fos)
                    }
                    return() # both in case of update and if fun matches
                }
            }
            ## either links correspond 1) to several existing fun.objects or 2) to
            ## one but with names mismatch, or 3) to zero. In 1-2): delete all
            ## existing fun.objects with a warning.
            if (length(fos) > 1L) {
                warning(paste0(links, collapse=', '),
                        ' correspond to several fun.objects: ',
                        paste0(fos.print, collapse=', '),
                        immediate. = TRUE) # prints warning immediately
            } else if (length(fos) == 1L) { # there should be names mismatch,
                ## otherwise this case was considered above
                warning('given names ', paste0(links, collapse=', '),
                        ' do not match the old names of fun.object ',
                        fos.print,
                        immediate. = TRUE) # prints warning immediately
            }
            rm.fun.objects( fos )
            ## Create new
            f.env <- fun.env[[ links[1L] ]] <- new.env( parent = emptyenv() )
            f.env $ fun <- fun
            f.env $ links <- links
            f.env $ by.reference <- by.reference
            for (link in links) {
                link.env[[ link ]] <- links[1L]
            }
            create.fun.links( links[1L] )
        }

        ## Defines the disk file name format. The result is fully determined
        ## by the arguments, so this is an "external" function in the sense that
        ## it does not use any internal `fun.club` variables
        file.name <- function(fo, ind, i.link, file.ext) {
            file.path(dir,
                      file.ext,
                      paste0(fo, '_', ind, '_', i.link, '.', file.ext))
        }
                              
        ## "summarizes" all file names in the form
        ## dir/{ext1,ext2,...}/fo_ind_{1-nlinks}.{ext1,ext2,...}
        file.summary <- function(fo, ind) {
            all.exts <- {
                . <- sapply(fun.env[[ fo ]] [[ ind ]] $ file, '[[', 'ext')
                . <- unique( . )
                if (length(.) > 1) {
                    paste0('{', paste0(., collapse = ','), '}')
                } else
                    .
            }
            all.i.links <- {
                . <- length(fun.env[[ fo ]] $ links)
                if (. > 1) {
                    paste0('{1-', ., '}')
                } else
                    '1'
            }
            file.name(fo, ind,
                      i.link = all.i.links,
                      file.ext = all.exts)
        }

        ## delete generated object corresponding to `fo`, `ind` together with
        ## its dependencies. `indent` is made of double space " " replicated N
        ## times where N is the recursive level. If `parents` is NULL
        ## (default), the parent links (to be deleted) are taken from the
        ## object. Otherwise, in the case when the object generation fails and
        ## the parent links are not created, `parents` are taken from the
        ## `stack` top and feed to this function via this argument.
        rm.generated <- function(fo, ind, indent = '', parents = NULL,
                                 not.recursive = TRUE) {
            fo.ind <- c(fo, ind)
            o.name <- obj.name(fo, ind)
            if (verbose >= 2) {
                if (not.recursive) {
                    message(indent, 'deleting ', o.name, ' and dependencies')
                } else {
                    message(indent, o.name)
                }
            }
            f.env <- fun.env[[ fo ]]
            ## `parents` is not NULL when the function generation fails, then
            ## the object might be absent. Then, the environment `e` below
            ## does not exist, so skip this part associated with `e`. If
            ## `parents` is not NULL, the object should always exist.
            if (!exists(ind, envir = f.env, inherits = FALSE)) {
                if (is.null(parents))
                    stop('Internal error: ', o.name, ' does not exist')
            } else {
                e <- f.env[[ ind ]]
                ## delete dependencies
                ##
                ## if the object generation failed, so that `parents` is NULL:
                ## there are no children so no need to remove them.
                if (is.null( parents )) {
                    ##
                    ## after deleting one child the others can be deleted
                    ## (recursively) as well. Therefore, deletion below is
                    ## done one at a time, after that the list of children is
                    ## dynamically updated
                    while ({
                        ch <- e $ children
                        length(ch) > 0
                    }) {
                        rm.generated(fo  = ch[[ 1L ]] [[ 1L ]],
                                     ind = ch[[ 1L ]] [[ 2L ]],
                                     indent = paste0(indent, '  '),
                                     not.recursive = FALSE)
                    }
                }
                ## `parents` links will be deleted in the end, outside the
                ## scope of `e` environment
                parents <- e$parents
                ## delete from storage
                all.files <- sapply(seq_along( e$file ),
                                    function(i) {
                                        file.name(fo, ind, i, e$file[[ i ]] [['ext']])
                                    })
                not.deleted <- Filter(function(file) unlink( file ) != 0,
                                      all.files)
                if (length(not.deleted) > 0) {
                    warning(paste0(not.deleted, collapse = ', '),
                            if (length(not.deleted) == 1) ' file' else ' files',
                            ' can not be deleted')
                } else {
                    fs <- file.summary(fo, ind)
                    if (verbose >= 2 && length(fs) != 0)
                        message(indent, '|files ', fs)
                }
                ## finally, delete this leaf of the Directed Acyclic Graph
                ## (it becomes the leaf after deleting the children)
                rm(list = ind, envir = f.env)
            }
            rm_arg(arg.encoder, fo, as.integer(ind)) # delete from C++ arg encoder
            for (p in parents) {
                . <- fun.env[[ p[1L] ]] [[ p[2L] ]]
                . $ children <- Filter(function(x) !identical(x, fo.ind),
                                       . $ children)
            }                
            if (verbose >= 2) {
                if ((. <- obj.names( parents )) != '') {
                    message(indent, '| link',
                            if (length( parents ) > 1) 's', o.name,' <- ', .)
                }
                message(indent, 'deletion of ', o.name,
                        ' is finished')
            }
        }

        ## returns a `list` of given `func` function arguments transmitted in
        ## `...`. The latter, in turn, can include named, positional and `...`
        ## arguments. The returned `list` is independent of the way of
        ## transferring the arguments if the resulting combination is the
        ## same.
        arg.list <- function(...) {
            f <- function() {
                ## all arguments except in `...`, ie. explicitly named by the
                ## caller or positional, appear in the environment of the
                ## function. Those in `...` can be accessed via
                ## `list(...)`. The following constructs the full list of
                ## arguments:
                args <- as.list( environment(), all.names = TRUE )
                if ('...' %in% names( formals() )) {
                    ## the best way to remove '...', always works regardless of other
                    ## arguments (named or not) and whether '...' is present or absent
                    args['...'] <- NULL
                    args <- c(args, list(...))
                }
                if (! is.null( names( args ))) {
                    ## sort arguments, so that the order of named arguments becomes
                    ## invariant. Unnamed arguments are placed first and, since the
                    ## sort is stable, their relative order is preserved, so they can
                    ## be correctly used as positional arguments.
                    ##
                    ## Side note: `names(list(10,20)) = names(list()) = NULL`
                    ## while `names(list(a=1, 10, 20)) = c('a', '', '')`
                    args[ order(names(args)) ]
                } else {
                    args
                }
            }
            fo <- link.methods[['fun.object']]
            formals(f) <- formals( fun.env[[ fo ]] $ fun )
            f(...)
        }

        ## printable version of the list, eg. for `l = list(a=1, 2)` returns
        ## "a=1, 2"
        print.list <- function(l) {
            n <- names(l)
            ## use `deparse` as it is more general than `as.character`
            ## internally used for conversion in `paste0`
            l <- sapply(l,
                        function(x) {
                            . <- try( deparse( x ), silent = TRUE)
                            if (inherits(., "try-error")) {
                                'unprintable'
                            } else {
                                ## collapse in case deparse output is
                                ## split in parts
                                . <- paste0(., collapse='')
                                if (nchar( . ) > 50L) {
                                    . <- substr(., start = 1L, stop = 46L)
                                    paste0(., ' ...')
                                } else .
                            }
                        })
            if (is.null( n )) {
                paste0(l, collapse = ', ')
            } else {
                n <- ifelse(n == '', '', paste0(n, '='))
                paste0(n, l, collapse = ', ')
            }
        }

        ## vector of link names
        all.links <- function() list(names = ls(all.names=TRUE, link.env),
                                     envir = envir)

        ## todo: add docs on C++ fun.club:::add_arg and other C++ funcs
        ## --------------- Functions, "exported" to `fun.link`s ---------------
        set.link <- function(link) {
            link.methods[['fun.object']] <<- link[['fun.object']]
            link.methods[['i.link']]     <<- link[['i']]
        }
        ## returns the `i.link`-th element of the list object generated by
        ## common function `fo` using `...` as arguments. Retrieve it from
        ## disk if it is not in memory or generate if not on disk.
        generate <- function(...) {
            fo <- link.methods[['fun.object']]
            i.link <- link.methods[['i.link']]
            ##
            f.env <- fun.env[[ fo ]]
            links <- f.env $ links
            link <- links[ i.link ]
            args <- arg.list( ... )
            serialized.arg <- rawToChar( serialize( args, connection = NULL, ascii = TRUE ))
            cArgs <- print.list( args )
            l.name <- paste0(link, '[', cArgs, ']')
            indent <- paste0(rep('  ', stack $ len()), collapse='')
            ##
            ind <- add_arg(arg.encoder, fo, serialized.arg, cArgs)
            new <-               ind[['new']]
            ind <- as.character( ind[['i']] )
            fo.ind <- c(fo, ind)
            o.name <- obj.name(fo, ind)
            if (new) {
                stack $ push( list() )
                if (verbose >=2) {
                    if (indent == '') {
                        message(indent, 'generating ', o.name)
                    } else {
                        message(indent, o.name)
                    }
                }
                ## message(indent,'generating ',o.name)
                tryCatch({
                    o.env <- f.env [[ ind ]] <- new.env( parent = emptyenv() )
                    o.env $ parents  = list()
                    o.env $ children = list()
                    ## list of character string vectors, eg. list('link1' =
                    ## c('rds',pdf')):
                    o.env $ file = list()
                    o.env $ data.env = new.env( parent = emptyenv() )
                    e <- o.env $ data.env
                    if (f.env $ by.reference) {
                        f.env $ fun(..., output.env = e) # call
                        for (l in links) {
                            if (! exists(link, envir = e, inherits = FALSE) ) {
                                stop(o.name,
                                     ' function must store its result(s) in variable(s) `',
                                     fun.names(fo),
                                     '` in the environment given by the function ',
                                     'argument `output.env` like `output.env$',
                                     links [ length(links) ],
                                     ' = ...`.')
                            }
                        }
                    } else {
                        ## if there is only one associated fun.link, store
                        ## result directly. Otherwise there should be many
                        ## results, one for every link, in the form of a
                        ## common list. Unpack them from the list and store.
                        n.links <- length( links )
                        if (n.links == 1L) {
                            e[[ link ]] <- f.env $ fun(...) # call
                        } else {
                            res <- f.env $ fun(...) # call
                            if ( ! is.list( res ) ||
                                 n.links != length( res ) ) {
                                stop(indent, 'The result of ',o.name,
                                     ' must be a list of length ', n.links,
                                     ' according to a number of given names in ',
                                     fun.names( fo ))
                            } else {
                                for (i in seq_along(res)) {
                                    l <- links[[ i ]]
                                    e[[ l ]] <- res[[ i ]]
                                }
                            }
                        }
                    }
                    ##
                    parents <- stack $ unique.top()
                    ## unique: if the same object is used >1 times in generation
                    ## fill dependencies
                    o.env $ parents <- parents
                    for (p in parents) {
                        ## fill children links opposite to parents
                        . <- fun.env[[ p[1L] ]] [[ p[2L] ]] $ children
                        fun.env[[ p[1L] ]] [[ p[2L] ]] $ children <-
                                               unique(c(., list(fo.ind)))
                        ## see comments for stack $ unique.top() above about
                        ## applying `unique` to `list`
                    }
                    ## save to storage.
                    ##
                    ## Note, for lazy functions (eg. for `ggplot` plots) the
                    ## execution can be delayed until saving to storage and
                    ## all potential errors can be detected only at this
                    ## stage.
                    for (i in seq_along(links)) {
                        l <- links[ i ]
                        exts <- extension.selector( o.env $ data.env [[ l ]] )
                        if (length( exts ) > 0) {
                            for (ext in exts) {
                                ## save function is in [[ 1 ]]
                                savers[[ ext ]] [[ 1 ]] (
                                    object = o.env $ data.env [[ l ]],
                                    file = file.name(fo, ind, i, ext)
                                )
                            }
                            ## note, the list `o.env $ file` is unnamed and
                            ## can only be referenced by numbers. It has the
                            ## same ordering as `f.env $ links`. This means
                            ## that in the conventions used throughout the
                            ## code, its `i.link`-th element corresponds to
                            ## the `link` in the sense that `link == f.env $
                            ## links[ i.link ]`
                            ##
                            ## First extension will be used for retrieving,
                            ## its version is stored
                            o.env $ file[[ i ]] <-
                                list(ext = exts,
                                     version = restorer $ version( exts[ 1L ] ))
                        }
                    }
                    fs <- file.summary(fo, ind)
                    if (verbose >= 2) {
                        message(indent, o.name, ' is generated',
                                if (length( parents ) > 0) {
                                    paste0(' from: ', obj.names(parents))
                                },
                                if (fs != '')  {
                                    paste0(' and stored to ', fs)
                                })
                    }
                    stack $ pop()
                    ## delete the information on parents from the stack top
                    ## only in the very end, as it might be needed to remove
                    ## links in case of error
                }, error = function( e ) { # callback for error
                    message( indent, 'Error is detected when generating ', o.name )
                    ## clean in case something was created
                    rm.generated(fo, ind, indent = indent,
                                 parents = stack $ unique.top())
                    stack $ clear()   # reset,
                    stop( e )         # break recursion and propagate stop() to top
                }, interrupt = function( e ) { # callback for interrupt
                    ## clean in case something was created
                    rm.generated(fo, ind, indent = indent,
                                 parents = stack $ unique.top())
                    stack $ clear() # reset, break recursion and propagate stop()
                    ## in case of interrupt, stop with the text, not `e` as it
                    ## was for error above
                    stop( indent, 'Generation of ',o.name,' was interrupted' ) # to top
                })
            } else {
                o.env <- f.env [[ ind ]]
                if (is.null( o.env $ data.env [[ link ]] )) {
                    ## file[[ i.link ]] $ ext contains the vector of the file
                    ## name extensions, the first extension is used for
                    ## retrieving
                    ext <- o.env $ file [[ i.link ]] [['ext']] [1]
                    version <- o.env $ file [[ i.link ]] [['version']]
                    restorer.fun <- restorer $ fun( ext, version )
                    o.env $ data.env [[ link ]] <-
                        restorer.fun( file = file.name(fo, ind, i.link, ext) )
                    if (verbose >= 2)
                        message( indent, l.name,' is retrieved from storage' )
                } else {
                    if (verbose >= 2)
                        message( indent, l.name,' is recalled from memory' )
                }
            }
            if (stack $ len() > 0) { # called from fun.object with non empty stack
                stack $ add.to.top( fo.ind )
                if (verbose >= 3)
                    message(indent, 'current stack: ', stack $ to.char())
            }
            o.env $ data.env [[ link ]]
        }

        ## assign NULL to object in memory but keep it on disk
        rm.arguments.from.memory <- function(...) {
            fo <- link.methods[['fun.object']]
            i.link <- link.methods[['i.link']]
            f.env <- fun.env[[ fo ]]
            args <- arg.list( ... )
            serialized.arg <- rawToChar( serialize( args, connection = NULL, ascii = TRUE ))
            ind <- ind_arg(arg.encoder, fo, serialized.arg)
            link <- f.env $ links [ i.link ]
            l.name <- obj.name(fo, ind, link)
            if ( ind == 0 ) {
                warning(l.name,' does not exist')
            } else {
                e <- f.env[[ as.character(ind) ]]
                if (length( e$file ) == 0) {
                    warning(l.name,' is not saved to disk, nothing is done')
                } else {
                    if (is.null( e$data.env[[ link ]] )) {
                        warning(l.name,' is not in memory') 
                    } else {
                        e$data.env[[ link ]] <- NULL
                        ## note, if data.env were a list, to set the list
                        ## element to NULL, one should write `list[ link ] =
                        ## list(NULL)`, while `list[[ link ]] <- NULL` deletes
                        ## the list element, see `help('[[')`. For the
                        ## environment above `data.env[[ link ]] <- NULL` is
                        ## Ok, it sets `link` to `NULL` but not deletes it.
                        if (verbose >= 2)
                            message(l.name,' is deleted from memory ',
                                    'but kept on disk')
                    }
                }
            }
        }

        ## print function for `print.fun.link()`
        print.fun <- function(fo, i.link) {
            f.env <- fun.env[[ fo ]]
            print(f.env $ fun)
            if (length( f.env[['links']] ) > 1)
                print(paste0('[[', i.link, ']]'))
        }
        
        ## delete object generated by `fo` with arguments in `...` and all
        ## dependencies
        rm.arguments <- function(...) {
            fo <- link.methods[['fun.object']]
            args <- arg.list( ... )
            serialized.arg <- rawToChar( serialize( args, connection = NULL, ascii = TRUE ))
            ind <- ind_arg(arg.encoder, fo, serialized.arg)
            if (ind == 0) {
                cArgs <- print.list( args )
                stop('The object ', fun.names(fo), '[', cArgs, ']', ' is not found')
            } else {
                rm.generated(fo, as.character(ind))
            }
        }
        
        save.on.exit <- function(e) {
            ## check whether `save.on.exit()` was already called: in this case
            ## `file` is already removed, then no need to save.  This happens
            ## when save.on.exit() is called from `unload()` via `save()`.
            ##
            if (!exists('file', envir = e, inherits = FALSE) ) return(NULL)
            ## `unload` should save immediately, as the file might be read
            ## back already by the next command. Therefore one can not just
            ## remove `fun.club` object and rely on the garbage collector to
            ## call `save.on.exit`, this can be delayed. So, `unload` calls
            ## `save.on.exit` directly (via `save()`), and then gc calls it
            ## for the 2nd time. It seems I can not unregister this function
            ## after it is registered by reg.finalizer (can only re-register,
            ## ie. substitute it by another empty function, which is
            ## approximately equivalent to `return(NULL)` above).
            if (e $ verbose >= 2) {
                message('saving fun.objects ',
                        paste0(e$fun.names( ls(all.names=TRUE, e$fun.env) ),
                               collapse=', '),
                        ' to ', e$file)
            }
            ## do not save objects as they are already saved on disk, free
            ## memory and set them to NULL
            for (fo in ls(all.names = TRUE, e$fun.env)) {
                f.env <- e$fun.env[[ fo ]]
                all <- ls(all.names=TRUE, f.env)
                for (ind in grep('^[0-9]+$', all, value=TRUE)) {
                    d.env <- f.env[[ ind ]] [['data.env']]
                    for (l in ls(all.names=TRUE, d.env)) {
                        d.env[[ l ]] <- NULL
                    }
                }
            }
            ## `file` below is used only here, it is fixed as
            ## `dir`/fun.club.rds.  It will be deleted in environment `e`
            ## but a copy will be saved in the `environment()` of this
            ## function, so that `saveRDS()` knows where to save `fun.club`:
            f <- e$file
            ## do not save `make.fun.club()` function arguments. When
            ## reloaded, the new, potentially different arguments could be
            ## assigned
            rm( dir, file, envir, extension.selector, savers, verbose,
               envir = e )
            ##
            ## save C++ arg encoder which associates function arguments to
            ## unique integers. For that, dump C++ encoder to R
            ## list-of-lists-of (string - integer) pairs and save it under the
            ## same name `arg.encoder`
            e$arg.encoder <- dump_arg( e$arg.encoder )
            ## note, the full environment is saved, not `methods` as returned
            ## by `make.fun.club`
            saveRDS( e, file = f)
        }
        reg.finalizer(environment(), onexit = TRUE, f = save.on.exit)

        save <- function() save.on.exit( parent.env( environment()))
        ##
        ## link.methods are propagated to created links
        ##
        ## one could store the functions in a list, but here an environment is
        ## chosen instead of the list simply because then the functions are
        ## not shown by `str(some.link)`. In addition, note that contrary to
        ## lists there is not partial matching for environments in expressions
        ## with `$`, like 'env $ name`. For lists instead of `$` (where `list
        ## $ abcdef` can be shortened to `list $ ab` if there is no ambiguity
        ## with the shorter form `ab`), I preferably use `[[` which have no
        ## partial matching by default (and could be a little faster).
        link.methods <- new.env( parent = emptyenv() )
        link.methods[[ 'set.link' ]]                 <- set.link
        link.methods[[ 'generate' ]]                 <- generate
        link.methods[[ 'rm.arguments.from.memory' ]] <- rm.arguments.from.memory
        link.methods[[ 'rm.arguments' ]]             <- rm.arguments
        link.methods[[ 'print.fun' ]]                <- print.fun

        ## `methods` environment is returned
        ##
        methods <- new.env( parent = emptyenv() )
        methods[[ 'all.links' ]]                           <- all.links
        methods[[ 'rm.links' ]]                            <- rm.links
        methods[[ 'assign.fun.object' ]]                   <- assign.fun.object
        methods[[ 'check.fun.links.and.return.function' ]] <-
            check.fun.links.and.return.function
        methods[[ 'save' ]]                                <- save

        clean.env( environment())
        return( structure(class = 'fun.club', methods) )
    }
}

## to do: descsribe by.reference
##
## Note, `fun.club[x]` works, `x` is correctly accepted from `...` and does
## not conflict with `x` as another argument. The same is true for
## `fun.club[by.reference]` and `fun.club[character.only]` if one would ever
## need to use these names for function objects.
##

#' Creates function object
#' 
#' 
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
`[<-.fun.club` <- function(x, ..., value, by.reference = FALSE, character.only = FALSE) {
    links <- if (!character.only) {
                 as.character(substitute(list(...))[-1L])
             } else
                 sapply(unlist(list(...),
                               use.names = FALSE),
                        as.character,
                        USE.NAMES = FALSE)
    if ( is.null(value) ) { # delete fun.objects associated to links
        x $ rm.links( links )
    } else {
        x $ assign.fun.object( links, value, by.reference = by.reference )
    }
    x
}

#' Unloads `fun.club` from memory in the same way as when quitting R session
#'
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
unload <- function(fun.club.name) {
    links <- get(fun.club.name) $ all.links()
    rm(list = links[['names']], envir = links[['envir']])
    get(fun.club.name) $ save()
    rm(list = fun.club.name, envir = parent.frame())
    invisible(gc()) # gc(verbose = FALSE) still prints output
}

#' Prints function object
#'
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
`[.fun.club` <- function(x, link = NULL, character.only = FALSE) {
    if (is.null(link)) {
        ## to do: list all fun.objects in fun.club environment
    } else {
        if (!character.only) link <- as.character(substitute(link))
        x $ check.fun.links.and.return.function( link )
    }
}


#' @name Generates object
#' @title Generates object
#' 
#' @description `...` arguments are forwarded to the function corresponding to
#'     this `fun.link` unchanged. For that, the `sys.call()` expression is
#'     modified by removing `x` first argument and by changing the function
#'     name to `generate` of `fun.club` class (which will in turn forward
#'     `...` to the corresponding function), and then evaluated.
#'
#' There is a special case when the function corresponding to the `fun.link`
#' also has an `x` argument. Then, `x=something` given in `...` overwrites `x`
#' (originally bounded to the `fun.link` object). The code below recovers it,
#' however, as it is still saved as a first argument in the expression
#' returned by `sys.call()`. (Eg. for `a[x=1]` or, equivalently,
#' `[.fun.link`('a', x=1) `sys.call()` returns arguments `('a', x=1)`, and 'a'
#' can be recovered). So, the function object can use arbitrary names without
#' restrictions.
#' 
#' Note also, the `fun.club` functions in `link.methods`, like `generate`
#' below, receive only `...` as arguments. This is done on purpose. Eg.
#' suppose that `generate` additionally receives the function object name and
#' the link number, like in `generate <- function(..., fo, i.link)`. In this
#' case, if by chance the function object uses the same argument name `fo` or
#' `i.link`, and one calls eg. `link[a=1, b=2, fo=3]`, then this argument `fo
#' = 3` will be in conflict with the argument for the function object name. To
#' avoid this situation, all functions receiving arguments through `...`,
#' ie. `link.methods` and also `arg.list(...)`, do not receive anything else
#' except `...`, while the function object and the link number are propagated
#' externally via `link.methods[['fun.object']]` and
#' `link.methods[['i.link']]`.
#' 
#'
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
`[.fun.link` <- function(x, ...) {
    ## instead of this function, I'd like to call generate(...) with the same
    ## arguments and from the caller environment `parent.frame()`.
    cl <- sys.call()
    ## Structure of `cl:
    ## 1 = [.fun.link
    ## 2 = x
    ## 3,4,5... = ...  - take those args
    ## in case there is an `x=something` argument in `...`, `x` might be
    ## overwritten, restore it:
    x <- eval(cl[[ 2 ]], envir = parent.frame())
    ## and remove from `cl`:
    cl <- cl[ -2L ]
    cl[[ 1L ]] <- x[['link.methods']] [['generate']]
    x[['link.methods']] [['set.link']] ( x )
    ##
    ## When there are no arguments to the `fun.link` function, like in
    ## `link[]`, the resulting `cl` has in the end the `missing argument` -
    ## the "strange" but standard object with an empty name "".
    ##
    ## Short explanation of "missing argument": it can be obtained via the
    ## missing function argument as `quote(f(x=))[[2]]`. One can get exactly
    ## the same object in a simpler way as `quote(expr=)`. It can not be bound
    ## to a "normal" name, eg. "x": `x <- quote(expr=)` works but then `x`
    ## produces a cryptic error `argument "x" is missing, with no default`.
    ##
    ## This missing argument appears in the end of `cl` in `link[]`
    ## but not in `[.fun.link`(link) or `[`(link), though at least the first
    ## and the third should be equivalent. I do not know why this happens. As
    ## a workaround, in the following ALL such missing arguments are excluded
    ## from `cl`, including those which might appear in different ways
    ## eg. after `link[a=]`. (In the latter case there is only one missing arg
    ## appears, not two, and under the name `a`).
    ##
    ## In principle, such missing argument constructs `link[a=,b=]` should not
    ## be used as the function object arguments, so always removing them looks
    ## safe.
    ##
    missing.args <- Filter(function(i) identical(cl[[i]],
                                                 quote(expr=)),
                           seq_along(cl))
    if (length( missing.args ) > 0) cl <- cl[ -missing.args ]
    eval(cl, envir = parent.frame())
}

#' Removes generated object from memory but keeps it on disk
#'
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
`[[<-.fun.link` <- function(x, ..., value) {
    cl <- sys.call()
    ## Structure of `cl:
    ## 1 = [.fun.link
    ## 2 = x
    ## 3,4,5... = ...  - take those args
    ## last = value
    ## in case there is `x=something` argument in `...`, `x` might be
    ## overwritten, restore it:
    x <- eval(cl[[ 2 ]], envir = parent.frame())
    ## the same for `value`
    value <- eval(cl[[ length(cl) ]], envir = parent.frame())
    if (is.null(value)) { # delete object but keep it in storage
        ## remove both `x` and `value` from `cl`:
        cl <- cl[ -c(2L, length(cl)) ]
        cl[[ 1L ]] <- x[['link.methods']] [['rm.arguments.from.memory']]
        x[['link.methods']] [['set.link']] ( x )
        ## remove all missing arguments
        missing.args <- Filter(function(i) identical(cl[[i]], quote(expr=)), seq_along(cl))
        if (length( missing.args ) > 0) cl <- cl[ -missing.args ]
        eval(cl, envir = parent.frame())
    } else {
        warning('only NULL can be assigned to fun.object like ',
                'f[[...]] = NULL,\n',
                'in which case f[...] is deleted from memory but kept on disk')
    }
    x
}

#' Deletes `fun.link`
#' 
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
`[<-.fun.link` <- function(x, ..., value) {
    cl <- sys.call()
    ## Structure of `cl:
    ## 1 = [.fun.link
    ## 2 = x
    ## 3,4,5... = ...  - take those args
    ## last = value
    ## in case there is `x=something` argument in `...`, `x` might be
    ## overwritten, restore it:
    x <- eval(cl[[ 2 ]], envir = parent.frame())
    ## the same for `value`
    value <- eval(cl[[ length(cl) ]], envir = parent.frame())
    if (is.null(value)) { # recursively delete fun.object x[X]
        ## remove both `x` and `value` from `cl`:
        cl <- cl[ -c(2L, length(cl)) ]
        cl[[ 1L ]] <- x[['link.methods']] [['rm.arguments']]
        x[['link.methods']] [['set.link']] ( x )
        ## remove all missing arguments
        missing.args <- Filter(function(i) identical(cl[[i]], quote(expr=)), seq_along(cl))
        if (length( missing.args ) > 0) cl <- cl[ -missing.args ]
        eval(cl, envir = parent.frame())
    } else {
        warning('only NULL can be assigned to fun.object like f[X] = NULL,\n',
                'in which case f[X] is deleted with all dependencies')
    }
    x
}

## to do: should be the  same  as for fc[fo]
#' Prints function corresponding to the function object
#' 
#' @return `fun.link` invisibly
#' @author Vladislav BALAGURA <balagura@cern.ch>
#' @export
print.fun.link <- function(x) {
    x[['link.methods']] [['print.fun']]( x[['fun.object']], x[['i']] )
    invisible(x)
}


