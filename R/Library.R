
Library <- function (Package, Package.Name = NULL, attach = TRUE, update = FALSE, pos = 2, quiet = TRUE, warnPackageUpdate = TRUE, 
             force = FALSE, autoAddRepo = TRUE, INSTALL_opts = "", ...) {
   
    sourceFunctionURL <- function (URL) {
        " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
        require(httr)
        File.ASCII <- tempfile()
        on.exit(file.remove(File.ASCII))
        getTMP <- httr::GET(URL)
        write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
        source(File.ASCII, local = parent.env(environment()))
    }
	 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
    
    oldOpts <- options(download.file.method = "auto")  # Sometimes remotes::install_github() throws an error without this
    on.exit(options(oldOpts))
    
    if (autoAddRepo & options()$repos[names(options()$repos) %in% "CRAN"] %in% "@CRAN@") 
        local({
            r <- getOption("repos")
            r["CRAN"] <- "http://cran.fhcrc.org"
            options(repos = r)
        })
    
    if (!(is.character(substitute(Package)))) 
        Package <- deparse(substitute(Package))
        
    if (grepl("/", Package)) {
        if (any(utils::installed.packages()[, 1] %in% "remotes")) {
            if (update) 
                update.packages("remotes", ask = FALSE)
        } else 
            install.packages("remotes", quiet = quiet)
        
        if (!any(utils::installed.packages()[, 1] %in% "remotes")) 
            stop(paste("CRAN remotes package is not installed; an attempt to install failed (check for internet access)"))
            
        if (!(is.character(substitute(Package.Name)))) 
            Package.Name <- deparse(substitute(Package.Name))
            
        if (Package.Name == "NULL") 
            Package.Name <- get.subs(Package, "/")[[length(get.subs(Package, "/"))]]
            
		if (!quiet) 
	        on.exit(cat("\nPackage = ", Package, "; Package.Name = ", Package.Name, "\n\n", sep = ""), add = TRUE)
        
        if(!quiet) cat("\n")       
        SHAs <- JRWToolBox::local_and_remote_SHA(Package, quiet = quiet)
        if(!quiet) cat("\n")
        SHA_Local_Old <- SHAs[1]
        SHA_GitHub <- SHAs[2]
        
        if(!is.na(SHA_Local_Old) & SHA_Local_Old == SHA_GitHub & !force & !quiet) {
             update <- FALSE
             cat(paste0("\nNo update needed, local and remote SHA numbers are equal (Use force = TRUE to force an update.)\n\n"))
        }
        
        if(!is.na(SHA_Local_Old) & SHA_Local_Old != SHA_GitHub & !update & !quiet)     
             warning(paste0("R '", Package.Name, "' is newer on GitHub. (Use update = TRUE to update.)"))
             
        if(is.na(SHA_Local_Old) | update | force)     
             remotes::install_github(Package, quiet = quiet, force = force, INSTALL_opts = INSTALL_opts, ...)
          
        if (!any(utils::installed.packages()[, 1] %in% Package.Name)) 
             stop(paste0("R '", Package.Name, "' package from Github is not installed. Note that the R package name may not be the same\n", 
             "           as the GitHub directory name, if so, use the Package.Name argument. Find the R package name using quiet = FALSE."))
         
        if(!quiet) cat("\n")          
        SHAs <- JRWToolBox::local_and_remote_SHA(Package, quiet = quiet)
        if(!quiet) cat("\n")
        SHA_Local_New <- SHAs[1]
                       
        if (!is.na(SHA_Local_Old) & SHA_Local_Old == SHA_Local_New & !quiet)
            warning(paste0("R '", Package.Name, "' package's SHA number did not change."))
       
        if ((SHA_Local_Old != SHA_GitHub) & (SHA_Local_New == SHA_GitHub) & warnPackageUpdate)
            cat(paste0("\nThe '", Package.Name, "' package was updated to the latest version\n\n"))                 
        
        if (attach) {
            unloadNamespace(Package.Name)   
            library(Package.Name, pos = pos, character.only = TRUE)
            if(!quiet)
               cat(paste0("\n", Package.Name, " was placed in position ", pos, " of the search() path.\n\n"))
        } 
        
    } else {
        #  Downloading all of CRAN's packages via available.packages() is prohibitively time consuming, hence no check is done here.
        if (any(utils::installed.packages()[, 1] %in% Package)) {
        
            if (update) 
                update.packages(Package, ask = FALSE)
                
        } else 
            install.packages(Package, quiet = quiet, INSTALL_opts = INSTALL_opts, ...)
        
        if (!any(utils::installed.packages()[, 1] %in% Package)) 
            stop(paste("CRAN", Package, "package is not installed; an attempt to install failed (check for internet access)"))
            
        if (attach)  {
            unloadNamespace(Package)  
            library(Package, pos = pos, character.only = TRUE)
            if(!quiet)
               cat("\n", Package.Name, "was placed in position", pos, "of the search() path.\n\n")
        }
    }
	
}
