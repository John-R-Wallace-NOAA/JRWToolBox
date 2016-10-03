lib <- function(Package) {
  
    if (!(is.character(substitute(Package)))) 
       Package <- deparse(substitute(Package))

    if(!any(installed.packages()[,1] %in% Package))
       install.packages(Package)
    require(Package, character.only = TRUE)
}
