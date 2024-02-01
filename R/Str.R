Str <- function (object, N = 4, nrows = 7, ncols = 7, ...) 
{
    cat("\n\n")
    if (is.list(object) & is.data.frame(object)) {
        Cols <- min(c(ncols, ncol(object)))
        str(object[, 1:Cols], ...)
        cat("\n")
		print(object[1:min(c(nrows, nrow(object))), 1:Cols])
    }
    if (is.list(object) & !is.data.frame(object)) {
        str(object, ...)
        print(lapply(object, head, N))
    }
    if (!is.list(object) & !is.data.frame(object)) {
        str(object, ...)
        head(object, N)
    }
}
