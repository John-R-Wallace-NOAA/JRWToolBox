 
.onAttach <- function(libname, pkgname) {

    packageStartupMessage("#######################################################")
    # packageStartupMessage("Loading JRWToolbox and rgit packages - Welcome")
    packageStartupMessage("Loading JRWToolbox - Welcome")
    packageStartupMessage("#######################################################")
    
    if( getOption("repos")["CRAN"] == "@CRAN@" ) {
    
        options(repos=c(CRAN="https://cloud.r-project.org/", CRANextra = "http://lib.stat.cmu.edu/R/CRAN/"))
    }
    
    # if( !"rgit" %in% utils::installed.packages()[,1] ) {
    
    #   devtools::install_github("John-R-Wallace-NOAA/rgit")
  	 #   library(rgit, pos = 3)
  	  
    # } else {
  	  
    # if('package:rgit' %in% search())
    #    detach("package:rgit")
  	 # library(rgit, pos = 3)
    # }
}
