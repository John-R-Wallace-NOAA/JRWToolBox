 load <- function (file, str. = TRUE, list.len = 15, nrow = 5, ...) 
{
    ls.ext <- function(file, str. = TRUE, list.len, nrow) {
        local({
            base::load(file)
            if (str. == TRUE) {
                Names <- base::ls()
                for (i in Names) {
                  OBJ <- eval(parse(text = i))
                  cat("\n", i, ":\n\n", sep = "")
                  str(OBJ, list.len = list.len)
                  cat("\n")
                  if(is.matrix(OBJ) | is.data.frame(OBJ)) { 
                     print(head(OBJ, nrow)); flush.console(); cat("\n") 
                     print(dim(OBJ)); flush.console(); cat("\n")
                  }
                }
                rm(i, Names)
                invisible(base::ls())
            }
            else base::ls()
        })
    }
    base::load(file, .GlobalEnv, ...)
    ls.ext(file, str. = str., list.len = list.len, nrow = nrow)
}


