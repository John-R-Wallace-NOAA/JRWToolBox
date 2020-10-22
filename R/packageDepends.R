packageDepends <- function(package) {
   # From: https://stackoverflow.com/questions/14645363/listing-r-package-dependencies-without-installing-packages
   require(packrat)
   if (!(is.character(substitute(package)))) {
        package <- deparse(substitute(package))
   }     
   packrat:::recursivePackageDependencies(package,lib.loc = .libPaths()[1])
}  
   
