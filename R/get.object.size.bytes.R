

    get.object.size.bytes <- function(object.name, pos = -1) {
      '#  Changed from Arni Magnusson code'
         object <- get(object.name, pos = pos)
         size <- try(unclass(object.size(object)), silent = TRUE)
         if (class(size) == "try-error") 
            size <- 0
         size
    }
