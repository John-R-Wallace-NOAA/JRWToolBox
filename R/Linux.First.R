
Linux.First <- function() {

    ###########################################
        # Purpose: this is my Rprofile.site file
        # This is the global file that gets called when R sets up the working environment, each time it is opened
    ##########################################

    lib(utils)
     
    cat("\nChanging CRAN repository away from the Revolution Analytics frozen mirror (see .../MRO/MRO-X.X.X/etc/Rprofile.site).\n\n")
    options(width = 160, help_type = "html", stringsAsFactors = FALSE, 
	    repos=c(CRAN="http://www.stats.ox.ac.uk/pub/RWin", CRANextra = "https://cran.cnr.berkeley.edu"))
    
    switch(menu("Check for package updates?") + 1,
           cat("\n"), update.packages())
  
    switch(menu("Check for GitHub updates?") + 1,
           cat("\n"), { try(devtools::install_github("John-R-Wallace/R-ToolBox")); cat("\n\n");
                        try(devtools::install_github("John-R-Wallace/Imap")); cat("\n\n");
                        try(devtools::install_github("r4ss/r4ss")); cat("\n\n");
                        try(devtools::install_github("kaskr/adcomp/TMB")); cat("\n\n");
                        try(devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")); cat("\n\n");
                        try(devtools::install_github("kaskr/TMB_contrib_R/TMBdebug")); cat("\n\n");
                        try(devtools::install_github("kaskr/TMB_contrib_R/TMBphase")); cat("\n\n");
                        try(devtools::install_github("james-thorson/VAST")); cat("\n\n");
                        try(devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM")); cat("\n\n");
                        try(devtools::install_github("james-thorson/utilities")); cat("\n\n");
                        # try(devtools::install_github("james-thorson/MIST"))
                        cat("\n\n") })
  
                        # try(devtools::install_github("glmmTMB/glmmTMB",subdir="glmmTMB")); cat("\n\n")
                         
    switch(menu("Reinstall INLA, glmmTMB and rstan?") + 1,
           cat("\n"), {
                        try(install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")); cat("\n\n");
                        try(install.packages("glmmTMB", repos = "http://glmmtmb.github.io/glmmTMB/repos/")); cat("\n\n");
                        try(install.packages('rstan', repos = 'https://cloud.r-project.org/', dependencies = TRUE)); cat("\n\n")  
                      })
   
    cat("\nDone with package updates.\n"); flush.console()
    
  
    try(lib(datasets))
    try(lib(stats))
    try(lib(methods))
    try(lib(graphics))
    try(lib(chron))
    try(lib(foreign))
    try(lib(grid))
    try(lib(grDevices))
    try(lib(sp))
    # try(lib(rgeos))
    try(lib(lattice))
    try(lib(latticeExtra))
             
    try(lib(akima))
    
    try(lib(gdata))
    try(lib(gplots))
    try(lib(gstat))
    try(lib(gtools))
    # try(lib(stashR))
    try(lib(gam))
    try(lib(Hmisc))
    try(lib(coda))
    # try(lib(MCMCpack))
    try(lib(MASS))
    try(lib(RODBC))
    # try(lib(mvbutils))
    
    try(lib(mvtnorm))
    try(lib(numDeriv))
    try(lib(bbmle))
    try(lib(Matrix))
    try(lib(devtools))
    try(lib(data.table))
    try(lib(TMB))
    try(lib(TMBhelper))
    try(lib(TMBdebug))
    try(lib(r4ss))
  
    "Load packages when called"
  
    # coda	
    autoload("mcmc", "coda")
    autoload("as.mcmc", "coda")
    autoload("densplot2", "coda")
   
  	
    cat("\nDone with lib and autoloads.\n"); flush.console()  
  
    options(help_type = "html", stringsAsFactors = FALSE)
	
    # lattice::lattice.options(default.theme = "standard.theme")
   
    Sys.setenv("R_HISTSIZE"=99999) # Longest possible history file
  
   # .First on PC
       if(exists(".SavedPlots"))  rm(.SavedPlots, pos = 1) 		# if there is a plotting history, delete it
       if(exists(".Traceback", 1, inherits = FALSE))  rm(.Traceback, pos = 1) 	# if there is an error history, delete it
       # if(exists(".Random.seed")) { rm(.Random.seed, pos=1) } 	# deletes the random number seed if it exists
        
       try(lib(Imap))
       try(updateTools(force = TRUE)) # Move tools to position 2 on search path
             
       Mydata<- list(a=1)
       attach(Mydata,2)  
  
  
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
      
      # Save toolbox (including ASCII versions)
        saveToolbox()
      
      # Save history
      #  savehistory()
      
      
      # Commands to clean up the working environment for next session.
      
        rm(.SavedPlots, pos = 1) 	 	        # if there is a plotting history, delete it
        rm(.Traceback, pos = 1) 		 	        # if there is an error history, delete it
        # rm(.Random.seed,pos=1) 		        # deletes the random number seed if it exists
        rm(.Last, pos = 1) 			        # .Last file deletes itself
  }
  
  cat("\nDone with .Rprofile.\n")
  
  invisible()
  
}





