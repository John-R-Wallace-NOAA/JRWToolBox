Setwd <- function(dir) {

    HOME <<- getwd()
    goHome <<- as.function(alist(setwd(HOME)))
    setwd(dir)
}
    
