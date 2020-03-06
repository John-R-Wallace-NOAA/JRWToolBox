lib <- function (Package, Package.Name = NULL, attach = TRUE, updateCRAN = FALSE, 
    pos = 2, quiet = FALSE, warnPackageUpdateOnly = quiet, force = FALSE, autoAddRepo = TRUE, ...) 
{
   oldOpts <- options(download.file.method = "auto")  # Sometimes remotes::install_github() throws an error without this
   on.exit(options(oldOpts))
   
    " # quiet = ifelse(sys.nframe() < 2, FALSE, TRUE) "
    if (autoAddRepo & options()$repos[names(options()$repos) %in% "CRAN"] %in% "@CRAN@") 
        local({
            r <- getOption("repos")
            r["CRAN"] <- "http://cran.fhcrc.org"
            options(repos = r)
        })
    "  "
    if (!(is.character(substitute(Package)))) 
        Package <- deparse(substitute(Package))
        
    if (grepl("/", Package)) {
        if (any(utils::installed.packages()[, 1] %in% "remotes")) {
            if (updateCRAN) 
                update.packages("remotes", ask = FALSE)
        }
        else install.packages("remotes", quiet = quiet)
        
        if (!any(utils::installed.packages()[, 1] %in% "remotes")) 
            stop(paste("CRAN remotes package is not installed; an attempt to install failed (check for internet access)"))
            
        if (!(is.character(substitute(Package.Name)))) 
            Package.Name <- deparse(substitute(Package.Name))
            
        if (Package.Name == "NULL") 
            Package.Name <- get.subs(Package, "/")[[length(get.subs(Package, "/"))]]
            
		if(!quiet) 
	        on.exit(cat("\nPackage = ", Package, "; Package.Name = ", Package.Name, "\n\n", sep = ""), add = TRUE)
            
        if (any(utils::installed.packages()[, 1] %in% Package.Name))  
             SHA.OLD <- packageDescription(Package.Name)$RemoteSha 
             
        remotes::install_github(Package, quiet = quiet, force = force, ...)
        
        if (!any(utils::installed.packages()[, 1] %in% Package.Name)) 
            stop(paste0("R '", Package.Name, "' package from Github is not installed. Note that the R package name may not be the same \n            as the GitHub directory name, if so, use the Package.Name argument. Find the R package name using quiet = FALSE."))
        
        if(exists('SHA.OLD') && length(SHA.OLD) > 0) {
          if (packageDescription(Package.Name)$RemoteSha == SHA.OLD & !warnPackageUpdateOnly & !quiet)
              warning(paste0("R '", Package.Name, "' package's SHA number did not change,\nhowever the package was placed in position 2 of the search() path."))
          if (packageDescription(Package.Name)$RemoteSha != SHA.OLD & warnPackageUpdateOnly)
              warning(paste0("R '", Package.Name, "' package was updated to the latest version"))                 
        }
        
        if (attach) {
            unloadNamespace(Package.Name)   
            library(Package.Name, pos = pos, character.only = TRUE)
        }    
    } else {
        if (any(utils::installed.packages()[, 1] %in% Package)) {
            if (updateCRAN) 
                update.packages(Package, ask = FALSE)
        }
        else install.packages(Package, quiet = quiet, ...)
        
        if (!any(utils::installed.packages()[, 1] %in% Package)) 
            stop(paste("CRAN", Package, "package is not installed; an attempt to install failed (check for internet access)"))
            
        if (attach) 
            library(Package, pos = pos, character.only = TRUE)
    }
	
}


