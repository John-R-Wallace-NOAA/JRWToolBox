Library <- function (Package, Package.Name = NULL, attach = TRUE, update = "ask", updateGitHub = update, updateCRAN = update, pos = 2, quiet = FALSE, 
         warnPackageUpdateOnly = quiet, force = FALSE, autoAddRepo = TRUE, INSTALL_opts = "", ...) {
   
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
            if (updateCRAN) 
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
            
        if (any(utils::installed.packages()[, 1] %in% Package.Name))  
             SHA.OLD <- packageDescription(Package.Name)$RemoteSha 
        
        if(!is.logical(updateGitHub))
              updateGitHUb <- casefold(updateGitHub)  
              
        remotes::install_github(Package, quiet = quiet, force = force, INSTALL_opts = INSTALL_opts, update = updateGitHub, ...)
        
        if (!any(utils::installed.packages()[, 1] %in% Package.Name)) 
            stop(paste0("R '", Package.Name, "' package from Github is not installed. Note that the R package name may not be the same\n", 
            "           as the GitHub directory name, if so, use the Package.Name argument. Find the R package name using quiet = FALSE."))
        
        if (exists('SHA.OLD') && length(SHA.OLD) > 0) {
          if (packageDescription(Package.Name)$RemoteSha == SHA.OLD & !warnPackageUpdateOnly & !quiet)
              warning(paste0("R '", Package.Name, "' package's SHA number did not change, however the package was placed in position ", pos, 
              " of the search() path."))
              
          if (packageDescription(Package.Name)$RemoteSha != SHA.OLD & warnPackageUpdateOnly)
              warning(paste0("R '", Package.Name, "' package was updated to the latest version"))                 
        }
        
        if (attach) {
            unloadNamespace(Package.Name)   
            library(Package.Name, pos = pos, character.only = TRUE)
        }    
    } else {
        if (any(utils::installed.packages()[, 1] %in% Package)) {
             
            if (is.logical(updateCRAN)) {            
               if(updateCRAN)                               
                   update.packages(Package, ask = FALSE)
            } else {            
               if(casefold(updateCRAN) == "ask")
                   update.packages(Package, ask = TRUE)             
            }   
        } else 
            install.packages(Package, quiet = quiet, INSTALL_opts = INSTALL_opts, ...)
        
        if (!any(utils::installed.packages()[, 1] %in% Package)) 
            stop(paste("CRAN", Package, "package is not installed; an attempt to install failed (check for internet access)"))
            
        if (attach)  {
            unloadNamespace(Package)  
            library(Package, pos = pos, character.only = TRUE)
        }
    }
	
}
