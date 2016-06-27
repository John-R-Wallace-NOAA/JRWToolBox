mTidy <- function (..., what, envir = .GlobalEnv) 
{
    if (missing(what)) 
        what <- sapply(match.call(expand.dots = FALSE)$..., deparse)
    if (!length(what)) 
        return()
    envir <- as.environment(envir)
    mcache <- attr(envir, "mcache")
    if (!missing(what)) {
        what <- what %such.that% (. %in% objects(envir = .GlobalEnv, 
            all.names = T))
        mlazy(what = what %except% names(mcache), envir = envir)
    }
    else what <- names(mcache)
    if (!length(what)) 
        return(invisible(what))
    path <- attr(envir, "path")
    if (is.null(path)) 
        stop("environment has no path attribute")
    save.mchanged(what, envir)
    remove(list = what, envir = envir)
    setup.mcache(refs = what, envir = envir)
    invisible(what)
}
<environment: namespace:mvbutils>
