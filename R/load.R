load <- function (file, str. = TRUE, list.len = 15, nrow = 5, ncol = nrow, all.names = TRUE, ...) 
{
  '  # The existence of baseLoad is a flag for doing base::load() - used for Windows auto load on drag and drop  '
    ls.ext <- function(file, str. = TRUE, list.len, nrow, ncol, all.names = TRUE) {
        local({
            base::load(file)
            if (str. == TRUE) {
                Names <- base::ls(all.names = all.names)
                for (i in Names[!grepl("%", Names)]) {
                  OBJ <- eval(parse(text = i))
                  cat("\n", i, ":\n\n", sep = "")
                  str(OBJ, list.len = list.len)
                  cat("\n")
                  if (is.matrix(OBJ) | is.data.frame(OBJ)) {
                    print(OBJ[1:(ifelse(nrow(OBJ) <= 10, nrow(OBJ), nrow)), 1:(ifelse(ncol(OBJ) <= 10, ncol(OBJ), ncol))])
                    flush.console()
                    cat("\nDimension:", dim(OBJ), "\n\n")
                    flush.console()
                  }
                }
                rm(i, OBJ, Names)
                invisible(base::ls(all.names = all.names))
            }
            else base::ls(all.names = all.names)
        })
    }
    
    base::load(file, .GlobalEnv, ...)
    
    if(!exists('baseLoad') & !rev(JRWToolBox::get.subs(file, '\\'))[1] %in% c('.RData', '.Rhistory'))
       ls.ext(file, str. = str., list.len = list.len, nrow = nrow, ncol = ncol, all.names = all.names)
}
