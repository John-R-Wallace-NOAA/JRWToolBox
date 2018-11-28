lib <- function (Package, Package.Name = NULL, attach = TRUE, updateCRAN = FALSE, 
    pos = 2, quiet = ifelse(sys.nframe() < 2, FALSE, TRUE), force = FALSE, 
    autoAddRepo = TRUE, ...) 
{
    if (autoAddRepo & options()$repos[names(options()$repos) %in% 
        "CRAN"] %in% "@CRAN@") 
        local({
            r <- getOption("repos")
            r["CRAN"] <- "http://cran.fhcrc.org"
            options(repos = r)
        })
    "  "
    if (!(is.character(substitute(Package)))) 
        Package <- deparse(substitute(Package))
    if (grepl("/", Package)) {
        if (any(installed.packages()[, 1] %in% "remotes")) {
            if (updateCRAN) 
                update.packages("remotes", ask = FALSE)
        }
        else install.packages("remotes", quiet = quiet)
        if (!any(installed.packages()[, 1] %in% "remotes")) 
            stop(paste("CRAN remotes package is not installed; an attempt to install failed (check for internet access)"))
        if (!(is.character(substitute(Package.Name)))) 
            Package.Name <- deparse(substitute(Package.Name))
        if (Package.Name == "NULL") 
            Package.Name <- get.subs(Package, "/")[[length(get.subs(Package, "/"))]]
        if (any(installed.packages()[, 1] %in% Package.name))  
             SHA.OLD <- packageDescription(Package.name)$RemoteSha         
        unloadNamespace(Package.name)     
        remotes::install_github(Package, quiet = quiet, force = force)
        if (!any(installed.packages()[, 1] %in% Package.Name)) 
            stop(paste0("R '", Package.Name, "' package from Github is not installed. Note that the R package name may not be the same \n            as the GitHub directory name, if so, use the Package.Name argument. Find the R package name using quiet = FALSE."))
        if(packageDescription(Package.name)$RemoteSha == SHA.OLD)
           stop(paste0("R '", Package.Name, "' package's SHA number did not change"))        
        if (attach) 
            library(Package.Name, pos = pos, character.only = TRUE)
    }
    else {
        if (any(installed.packages()[, 1] %in% Package)) {
            if (updateCRAN) 
                update.packages(Package, ask = FALSE)
        }
        else install.packages(Package, quiet = quiet, ...)
        if (!any(installed.packages()[, 1] %in% Package)) 
            stop(paste("CRAN", Package, "package is not installed; an attempt to install failed (check for internet access)"))
        if (attach) 
            library(Package, pos = pos, character.only = TRUE)
    }
}
