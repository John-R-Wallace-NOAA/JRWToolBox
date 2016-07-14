dir.use <- function(path, recursive = FALSE, ...) {
     if(!dir.exists(path))
        dir.create(path, recursive = recursive, ...)
     path
}
