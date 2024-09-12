extractRData <- function(object, file) {
      '   # Function for extracting an object from a .RData file created by the R save() command   '
      '   # Inputs: quoted object name, quoted .RData file path  '
      e <- new.env()
      base::load(file=file, envir = e)
      return(get(object, envir = e, inherits = FALSE))
}
