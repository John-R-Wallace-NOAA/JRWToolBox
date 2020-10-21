packageDepends <- function(package) {
   require(packrat)
   if (!(is.character(substitute(package)))) {
        package <- deparse(substitute(package))
   }     
   packrat:::recursivePackageDependencies(package,lib.loc = .libPaths()[1])
}  
   
