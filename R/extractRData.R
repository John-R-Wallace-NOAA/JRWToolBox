extractRData <- function(object, file) {
      '   # Function for extracting an object from a .RData file created by Rs save() command   '
      '   # Inputs: object nam, RData file   '
      e <- new.env()
      base::load(file=file, envir = e)
      return(get(object, envir = e, inherits = FALSE))
}
