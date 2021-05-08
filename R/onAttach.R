
  
.onAttach <- function(lib, pkg) {

    packageStartupMessage("##################################################################################################")
    # packageStartupMessage(paste0("Attaching JRWToolbox and rgit packages - Welcome"))
    packageStartupMessage(paste0("Attaching JRWToolbox - Welcome"))
  
    # Set max threads (or less if desired) if using R-MKL on Windows [depending on the system, threads (or cores) are often half the number of logical processors on a machine].    
    if(.Platform$OS.type == "windows") {

         if(any(installed.packages()[, 1] %in% "RhpcBLASctl"))  {  
		    packageStartupMessage("")
            RhpcBLASctl::blas_set_num_threads(RhpcBLASctl::get_num_cores()) # RhpcBLASctl::get_num_cores() gives max # of cores on a machine
            # RhpcBLASctl::blas_get_num_procs() Gives current numnber of threads set (regardless of the 'procs' name)
            packageStartupMessage(paste0("\nR-MKL (Intel libraries) on Windows, version: ", getRversion(), " with threads set to the machine maximum of: ", RhpcBLASctl::blas_get_num_procs()))
            
        } else
            packageStartupMessage(paste0("R on Windows, version: ", getRversion()))
    }
    
    if(.Platform$OS.type != "windows") {
        
        # Set 6 threads if using MRO on, e.g., a Linux server running CentOS (e.g. Tantalus), (setting the max too high results in too much IO, which is slow).
        if(any(installed.packages()[, 1] %in% "RevoUtilsMath")) {
           RevoUtilsMath::setMKLthreads(6)
           packageStartupMessage(paste0("MRO (Microsoft R Open) on a non-Windows machine, version: ", getRversion(), " with threads set to: ", RevoUtilsMath::getMKLthreads()))
        } 
        
        # Set 6 threads if using R-MKL on, e.g., a Linux server running CentOS (e.g. Tantalus), (setting the max too high results in too much IO, which is slow).
        if(any(installed.packages()[, 1] %in% "RhpcBLASctl")) {
            RhpcBLASctl::blas_set_num_threads(6)
            packageStartupMessage(paste0("R-MKL (Intel libraries) on a non-Windows machine, version: ", getRversion(), " with threads set to: ", RhpcBLASctl::blas_get_num_procs()))
        }
        
        if(!(any(installed.packages()[, 1] %in% "RevoUtilsMath") | any(installed.packages()[, 1] %in% "RhpcBLASctl"))) {
            packageStartupMessage(paste0("R on a non-Windows machine, version: ", getRversion(), "\n"))
        }
    
    }

    
  # notify the option to update? (5 possible cases)  - This section is from https://github.com/jinkim3/kim/R/onAttach.R
  # 1. error in getting the current package version -> yes
  # 2. error in getting the github package version -> yes
  # 3. current package version < github package version -> yes
  # 4. current package version > github package version -> no
  # 5. current package version == github package version -> no
  # in short, notify the option to update unless the version numbers match

  # get version of the currently installed package
  current_pkg_version <- tryCatch(
    as.character(utils::packageVersion("JRWToolBox")),
    error = function(e) "unknown")
	  
  # github url
  github_url <- paste0("https://raw.githubusercontent.com/John-R-Wallace-NOAA/", "JRWToolBox/master/DESCRIPTION")
	  
  # get github description or handle errors
  github_pkg_desc <- tryCatch(
    readLines(github_url),
    warning = function(w) {"github_desc_read_fail"},
    error = function(e) {"github_desc_read_fail"})
	  
  # get the version number of github version
  if (identical(github_pkg_desc, "github_desc_read_fail")) {
    github_pkg_version <- "unknown"
  } else {
    github_pkg_version <- gsub(
      ".*ersion: ", "", github_pkg_desc[
        grep("ersion", github_pkg_desc)])
  }
	  
  # compare versions
  compare_version_result <- tryCatch(
    utils::compareVersion(
      current_pkg_version, github_pkg_version),
    warning = function(w) {999}, # 999 indicates no need for update
    error = function(e) {999})
	  
  # skip update for case 5
  if (current_pkg_version != "unknown" &
      github_pkg_version != "unknown" &
      compare_version_result == 0) {
    startup_message <- paste0(
      "Package attached: JRWToolBox v", current_pkg_version,
      " (same as the most recent version available through GitHub).")
  } else if (
	  
    # skip update for case 4
    current_pkg_version != "unknown" &
    github_pkg_version != "unknown" &
    compare_version_result > 0) {
    startup_message <- paste0(
      "Package attached: JRWToolBox v", current_pkg_version,
      " (probably the most recent version available through GitHub).")
  } else {
	  
    # simply notify of the OPTION to update the package
    # this is simply a notification of the option to update,
    # rather than a recommendation to update
	  
    startup_message <- paste0(
      "Package attached: JRWToolBox v", current_pkg_version,
      "; Most recent version available on GitHub: v", github_pkg_version,
      "\n\nYou have an option to update the package ",
      "with the function `updateTools()`. ",
      "If you do so, make sure to restart R.\n")
  }
  packageStartupMessage(startup_message)
	 
  packageStartupMessage(paste0("\nNote the gitAFile() function has been moved to the 'rgit' package (see the repo for more info).\nPlease install the rgit package using:\n\n\t remotes::install_github('John-R-Wallace-NOAA/rgit')\n"))
  packageStartupMessage(paste0("\n(Installing GitHub R packages using .onAttach() is very problematic and may not be possible.)"))
  packageStartupMessage("##################################################################################################")
  
  
  if(FALSE) {
	
      # Use setHook() to run this code after the package environment is sealed, see the order of events in the help for setHook()
      setHook(
        packageEvent(pkg, "attach"),
           {
              if( getOption("repos")["CRAN"] == "@CRAN@" ) {
                  options(repos=c(CRAN="https://cloud.r-project.org/", CRANextra = "http://lib.stat.cmu.edu/R/CRAN/"))
              }
              
              if( !"rgit" %in% utils::installed.packages()[,1] ) {
                  devtools::install_github("John-R-Wallace-NOAA/rgit")
              } else {
                  if('package:rgit' %in% search())
                      detach("package:rgit")
              }
              library(rgit, pos = 3) 
           } 
      )
 } 
}


