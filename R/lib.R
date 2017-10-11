lib <- function(Package, require = TRUE) {
  
    if (!(is.character(substitute(Package)))) 
       Package <- deparse(substitute(Package))

    if(!any(installed.packages()[,1] %in% Package))
       install.packages(Package)
    if(require)
       require(Package, character.only = TRUE)
}
