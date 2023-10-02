resave <- 
function (..., list = character(), file, overwrite = TRUE, loadverbose = FALSE) 
{
    '   # From cgwtools R packcage on CRAN  '
    filnam = deparse(substitute(file))
    names <- as.character(substitute(list(...)))[-1]
    if (missing(list) && !length(names)) 
        stop("nothing specified to be save()d")
    list <- c(list, names)
    if (!file.exists(file)) 
        message(filnam, " does not exist. A new file will be created.")
    data_env <- new.env()
    base::load(file = file, envir = data_env, verbose = loadverbose)
    overlap <- list[list %in% ls(data_env)]
    if (length(overlap) > 0) 
        if (overwrite) {
            warning("The following objects will be overwritten in", 
                filnam, ": ", paste0(overlap, coll = " "))
        }
        else {
            warning("These items will not overwrite current objects in ", 
                filnam, ": ", paste0(overlap, coll = " "))
            list <- list[!(list %in% overlap)]
        }
    ok <- vapply(list, exists, NA)
    if (!all(ok)) {
        warning("Items not found: ", paste0(names(ok[!ok]), coll = ""))
        list <- list[ok]
    }
    for (obj in list) assign(x = obj, value = get(obj), envir = data_env)
    save(list = ls(data_env), file = file, envir = data_env)
}
