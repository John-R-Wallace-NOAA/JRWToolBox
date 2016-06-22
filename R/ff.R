ff <- function(f) {

  require(codetools)  

  leaf <- function (e, w) {
    r <- try(eval(e), silent = TRUE)
    if(!is.null(r) && is.function(r)) ret <<- c(ret, as.character(e))
  }
  call <- function (e, w) {
    walkCode(e[[1]], w)
    for (a in as.list(e[-1])) if (!missing(a)) walkCode(a, w)
  }
  ret <- c()
  walkCode(body(f), makeCodeWalker(call = call, leaf = leaf, write = cat))
  unique(ret)
}
