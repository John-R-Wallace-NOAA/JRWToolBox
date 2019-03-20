Source <- function (file, ...) 
{
    ls.ext <- function(file) {
        local({
            base::source(file, TRUE)
            base::ls()
        })
    }
    base::source(file, ...)
    ls.ext(file)
}


