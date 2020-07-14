
Linux.First <- function(askCRAN = TRUE) {

    ###########################################
        # Purpose: this is my Rprofile.site file
        # This is the global file that gets called when R sets up the working environment, each time it is opened
    ##########################################

    lib(utils)
     
    cat("\nChanging CRAN repository away from the Revolution Analytics frozen mirror (see .../MRO/MRO-X.X.X/etc/Rprofile.site).\n\n")
    options(width = 160, help_type = "html", stringsAsFactors = FALSE, 
	    repos=c(CRAN="https://cloud.r-project.org/", CRANextra = "http://lib.stat.cmu.edu/R/CRAN/"))
    
    switch(menu("Check for package updates?") + 1,
           cat("\n"), update.packages(ask = askCRAN))
  
   #  With the 'INSTALL_opts' argument, remotes::install_github() throws a warning when the SHA number doesn't change    
   switch(menu("Check for GitHub updates?") + 1,
          cat("\n"), { try(remotes::install_github("John-R-Wallace-NOAA/JRWToolBox")); cat("\n\n");
  	              try(remotes::install_github("John-R-Wallace-NOAA/rgit")); cat("\n\n");
                       try(remotes::install_github("John-R-Wallace-NOAA/Imap")); cat("\n\n");
                       try(remotes::install_github("r4ss/r4ss")); cat("\n\n");                      
                       try(remotes::install_github("kaskr/TMB_contrib_R/TMBhelper")); cat("\n\n");
                       try(remotes::install_github("kaskr/TMB_contrib_R/TMBdebug")); cat("\n\n"); 
                       try(remotes::install_github("kaskr/TMB_contrib_R/TMBphase")); cat("\n\n");
  				       try(remotes::install_github("mlysy/TMBtools")); cat("\n\n");
                       try(remotes::install_github("kaskr/adcomp/TMB")); cat("\n\n");
                       try(remotes::install_github("james-thorson-noaa/FishStatsUtils", INSTALL_opts="--no-multiarch --no-test-load")); cat("\n\n");
                       try(remotes::install_github("james-thorson/utilities")); cat("\n\n");
                       try(remotes::install_github("james-thorson-noaa/VAST", INSTALL_opts = "--no-multiarch --no-test-load --no-staged-install")); cat("\n\n");
                       try(remotes::install_github("nwfsc-assess/geostatistical_delta-GLMM")); cat("\n\n");
                       # try(remotes::install_github("james-thorson/MIST"))
                       cat("\n\n") })
  
                     # try(remotes::install_github("glmmTMB/glmmTMB",subdir="glmmTMB")); cat("\n\n")
                        
   switch(menu("Reinstall INLA?") + 1,
          cat("\n"), { 
  	 try(install.packages("BiocManager")); cat("\n\n");
   		 try(BiocManager::install(version = "3.11")); cat("\n\n");
  	 try(BiocManager::install(c("graph", "Rgraphviz"))); cat("\n\n");
  	 try(install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)); cat("\n\n") 
  	 })
    
   # OLD: gives version 0.0.2: install.packages("glmmTMB", repos = "http://glmmtmb.github.io/glmmTMB/repos/") => http://glmmtmb.github.io/glmmTMB/repos/src/contrib/glmmTMB_0.0.2.tar.gz
   switch(menu("Reinstall glmmTMB?") + 1,
          cat("\n"), { try(install.packages("glmmTMB")); cat("\n\n") })
  
   # switch(menu("Reinstall rstan?") + 1,
   #      cat("\n"), { try(install.packages('rstan', repos = 'https://cloud.r-project.org/', dependencies = TRUE)); cat("\n\n") })                    
   
  
   switch(menu("Reinstall compiled rstan?") + 1,
          cat("\n"), { try(install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)); cat("\n\n") })
   
   switch(menu("Reinstall rstan from source?") + 1,
          cat("\n"), { try({
          
              pkgbuild::has_build_tools(debug = TRUE) 
              dotR <- file.path(Sys.getenv("HOME"), ".R")
              if (!file.exists(dotR)) dir.create(dotR)
              M <- file.path(dotR, "Makevars.win")
              if (!file.exists(M)) file.create(M)
              cat("\nCXX14FLAGS=-O3 -march=native", "CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y", "CXX11FLAGS=-O3 -march=corei7", file = M, sep = "\n", append = TRUE)
              on.exit(file.remove(M))
              Sys.setenv(MAKEFLAGS = "-j4")
              install.packages("rstan", type = "source")
              # lib("stan-dev/rstan", subdir = "rstan/rstan") # Failed
         
              }); cat("\n\n") })                              
   
    cat("\nDone with package updates.\n"); flush.console()
    
   lib(datasets)
   lib(stats)
   lib(methods)
   lib(graphics)
   lib(chron)
   lib(foreign)
   lib(grid)
   lib(grDevices)
   lib(sp)
   lib(rgeos)
   lib(lattice)
   lib(latticeExtra)
             
   lib(akima)
    
   lib(gdata)
   lib(gplots)
   lib(gstat)
   lib(gtools)
   # lib(stashR)
   lib(gam)
   # lib(Hmisc)
   lib(coda)
   # lib(MCMCpack)
   lib(MASS)
   lib(RODBC)
   # lib(mvbutils)
    
   lib(mvtnorm)
   lib(numDeriv)
   lib(bbmle)
   lib(Matrix)
   lib(devtools)
   lib(data.table)
   lib(TMB)
   lib(TMBhelper)
   
   lib(RhpcBLASctl)
   
   lib(Imap)
   
   lib(rgit)
   repoList <- c("John-R-Wallace-NOAA/JRWToolBox", "John-R-Wallace-NOAA/rgit", "John-R-Wallace-NOAA/VAST_Examples_and_Scripts", "John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST")
   repoPath <- repoList[1]
   gitName <- "John-R-Wallace-NOAA"
   gitEmail <- "John.Wallace@noaa.gov"
      
   detach("package:JRWToolBox")
   library(JRWToolBox)

  
    # Load packages when called
  
    # coda	
    autoload("mcmc", "coda")
    autoload("as.mcmc", "coda")
    autoload("densplot2", "coda")
   
  	
    cat("\nDone with lib and autoloads.\n"); flush.console()  
 
    # lattice::lattice.options(default.theme = "standard.theme")
   
    Sys.setenv("R_HISTSIZE"=99999) # Longest possible history file
  
    # .First on PC
       if(exists(".SavedPlots"))  rm(.SavedPlots, pos = 1) 		# if there is a plotting history, delete it
       if(exists(".Traceback", 1, inherits = FALSE))  rm(.Traceback, pos = 1) 	# if there is an error history, delete it
       # if(exists(".Random.seed")) { rm(.Random.seed, pos=1) } 	# deletes the random number seed if it exists
   
       Mydata <- list(a = 1)
       attach(Mydata, 2)     
	   
       try(lib(Imap))
	   detach(pos = grep('JRWToolBox', search())) 
	   library(JRWToolBox, pos = 3)

       # try(updateTools(quiet = TRUE, force = TRUE)) # Move tools to position 2 on search path
             
      
  
  .Last <<- function () {
  
      ###########################################
          # Purpose: this is my core .Last file
          # History: John Wallace Jan, 2009
      ###########################################
  
  
      # Save backup of my session
        save.image(file=paste("BACKUP ", paste(strsplit(date(), ":")[[1]],
      	collapse ="."), ".RData"))
      
      # Save ASCII version of all functions
        save.functions()
           
      # Save history
      # savehistory()
      
      
      # Commands to clean up the working environment for next session.
      
        rm(.SavedPlots, pos = 1) 	 	        # if there is a plotting history, delete it
        rm(.Traceback, pos = 1) 		 	        # if there is an error history, delete it
        # rm(.Random.seed,pos=1) 		        # deletes the random number seed if it exists
        rm(.Last, pos = 1) 			        # .Last file deletes itself
  }
  
  cat("\nDone with .Rprofile.\n")
  
  invisible()
  
}








