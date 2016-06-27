SAVE.IMAGE <- function() { 
      save(list = base::ls(1, all=TRUE), file = ".RData") 
    }
