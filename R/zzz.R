
  
.onAttach <- function(libname, pkgname) {

    packageStartupMessage("###################################################################################################")
    # packageStartupMessage("Loading JRWToolbox and rgit packages - Welcome")
    packageStartupMessage("Loading JRWToolbox - Welcome")
    packageStartupMessage(paste0("This is R version: ", getRversion(),  "\n"))
  
    # Set max threads (or less if desired) if using R-MKL on Windows [depending on the system, threads (or cores) are often half the number of logical processors on a machine].    
    if(.Platform$OS.type == "windows") {

         if(any(installed.packages()[, 1] %in% "RhpcBLASctl"))  {  
            RhpcBLASctl::blas_set_num_threads(RhpcBLASctl::get_num_cores()) # RhpcBLASctl::get_num_cores() gives max # of cores on a machine
            # RhpcBLASctl::blas_get_num_procs() Gives current numnber of threads set (regardless of the 'procs' name)
             packageStartupMessage(paste0("R-MKL (Intel libraries) on Windows, version: ", getRversion(), "with threads set to the maximum of: ", RhpcBLASctl::blas_get_num_procs(), "\n"))
            
        } else
            packageStartupMessage(paste0("R on Windows, version: ", getRversion(), "\n"))
    }
    
    if(.Platform$OS.type != "windows") {
        
        # Set 6 threads if using MRO on, e.g., a Linux server running CentOS (e.g. Tantalus), (setting the max too high results in too much IO, which is slow).
        if(any(installed.packages()[, 1] %in% "RevoUtilsMath")) {
           RevoUtilsMath::setMKLthreads(6)
           packageStartupMessage(paste0("MRO (Microsoft R Open) on a non-Windows machine, version: ", getRversion(), "with threads set to: ", RevoUtilsMath::getMKLthreads(), "\n"))
        } 
        
        # Set 6 threads if using R-MKL on, e.g., a Linux server running CentOS (e.g. Tantalus), (setting the max too high results in too much IO, which is slow).
        if(any(installed.packages()[, 1] %in% "RhpcBLASctl")) {
            RhpcBLASctl::blas_set_num_threads(6)
            packageStartupMessage(paste0("R-MKL (Intel libraries) on a non-Windows machine, version: ", getRversion(), "with threads set to: ", RhpcBLASctl::blas_get_num_procs(), "\n"))
        }
        
        if(!(any(installed.packages()[, 1] %in% "RevoUtilsMath") | any(installed.packages()[, 1] %in% "RhpcBLASctl")) {
            packageStartupMessage(paste0("R on a non-Windows machine, version: ", getRversion(), "\n"))
        }
    
    }

     packageStartupMessage("###################################################################################################")
    
    
    if( getOption("repos")["CRAN"] == "@CRAN@" ) {
    
        options(repos=c(CRAN="https://cloud.r-project.org/", CRANextra = "http://lib.stat.cmu.edu/R/CRAN/"))
    }
    
    # if( !"rgit" %in% utils::installed.packages()[,1] ) {
    
    #    devtools::install_github("John-R-Wallace-NOAA/rgit")
      #    library(rgit, pos = 3)
        
    # } else {
        
    # if('package:rgit' %in% search())
    #    detach("package:rgit")
      #    library(rgit, pos = 3)
    # }
}


