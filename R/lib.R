lib <- function (Package, Package.Name = NULL, require = TRUE, quiet = TRUE, 
    force = FALSE, autoAddRepo = TRUE, ...) 
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
        if (any(installed.packages()[, 1] %in% "devtools")) {
            update.packages("devtools", ask = FALSE)
        }
        else install.packages("devtools", quiet = quiet)
        if (!any(installed.packages()[, 1] %in% "devtools")) 
            stop(paste("CRAN devtools package is not installed; an attempt to install failed (check for internet access)"))
        if (require) 
            require("devtools", character.only = TRUE)
        if (!(is.character(substitute(Package.Name)))) 
            Package.Name <- deparse(substitute(Package.Name))
        if (Package.Name == "NULL") 
            Package.Name <- get.subs(Package, "/")[[length(get.subs(Package, "/"))]]
        devtools::install_github(Package, quiet = quiet, force = force)
        if (!any(installed.packages()[, 1] %in% Package.Name)) 
            stop(paste0("R '", Package.Name, "' package from Github is not installed. Note that the R package name may not be the same \n            as the GitHub directory name, if so, use the Package.Name argument. Find the R package name using quiet = FALSE."))
        if (require) 
            require(Package.Name, character.only = TRUE)
    }
    else {
        if (any(installed.packages()[, 1] %in% Package)) {
            update.packages(Package, ask = FALSE)
        }
        else install.packages(Package, quiet = quiet, ...)
        if (!any(installed.packages()[, 1] %in% Package)) 
            stop(paste("CRAN", Package, "package is not installed; an attempt to install failed (check for internet access)"))
        if (require) 
            require(Package, character.only = TRUE)
    }
}


