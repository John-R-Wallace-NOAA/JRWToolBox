  
.onAttach <- function(lib, pkg) {

    local_and_remote_SHA <- function(repo,
                               ref = "HEAD",
                               subdir = NULL,
                               auth_token = remotes:::github_pat(quiet),
                               host = "api.github.com",
                               dependencies = NA,
                               upgrade = c("default", "ask", "always", "never"),
                               force = FALSE,
                               quiet = FALSE,
                               build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                               build_manual = FALSE, build_vignettes = FALSE,
                               repos = getOption("repos"),
                               type = getOption("pkgType"),
                               ...) {
       # require(remotes)                       
         
        remote <- remotes:::github_remote(repo, ref = ref, subdir = subdir, auth_token = auth_token, host = host)
    
        stopifnot(remotes:::is.remote(remote))
        package_name <- remotes:::remote_package_name(remote)
        local_sha <- remotes:::local_sha(package_name)
        remote_sha <- remotes:::remote_sha(remote, local_sha)
        c(local_sha = local_sha , remote_sha = remote_sha)
    }


    packageStartupMessage("##################################################################################################\n")
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

   SHAs <- local_and_remote_SHA( "John-R-Wallace-NOAA/JRWToolBox")
   
   if(SHAs[1] == SHAs[2]) 
       packageStartupMessage("\n\nThe local package is up-to-date with the repo on Github.\n")   
   else
      packageStartupMessage("\n\nThe local package is behind the repo on GitHub.\n\n", 
          "The library can be updated with this package's updateTools() function.\n", 
          "If the library is locked, you may have to restart R before using updateTools().\n")
   
   packageStartupMessage(paste0("\nNote the gitAFile() function has been moved to the 'rgit' package (see the repo for more info).\n",  
       "Please install the rgit package using:\n\n\t remotes::install_github('John-R-Wallace-NOAA/rgit')\n"))
       
   packageStartupMessage(paste0("\n(Installing GitHub R packages using .onAttach() is very problematic and may not be possible.)"))
   
   packageStartupMessage(paste0("\n\nThis package's 'load' function over-loads base::load() to enable the automatic viewing of\n",  
       "drag-and-drop '.RData' files. (FYI, drag-and-drop doesn't work in RStudio.)\n"))
       
   packageStartupMessage(paste0("The existence of the R object 'baseLoad' (e.g. baseLoad <- \"\") is a flag to revert back to\n", 
       "base::load() for drag-and-drop, since JRWToolBox::load() can be slow for very large '.RData' files.\n"))
       
   packageStartupMessage("##################################################################################################")
  
 }

