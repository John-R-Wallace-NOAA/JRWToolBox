lib <- function (Package, Package.Name = NULL, require = TRUE, quiet = TRUE, force = FALSE, autoAddRepo = TRUE) 
{
 
if(autoAddRepo & options()$repos[names(options()$repos) %in% 'CRAN'] %in% "@CRAN@") 	 
       local({getOption("repos") -> r; r["CRAN"] <- "http://cran.fhcrc.org"; options(repos = r)})	 
'  '
	
    if (!(is.character(substitute(Package)))) 
        Package <- deparse(substitute(Package))

	if(grepl("/", Package)) {
	     if (any(installed.packages()[, 1] %in% "devtools")) {
	      	    update.packages("devtools", ask = FALSE)
	      	 }  else 
	      	    install.packages("devtools", quiet = quiet)
         if (!any(installed.packages()[, 1] %in% "devtools"))
	                     stop(paste('CRAN devtools package is not installed; an attempt to install failed (check for internet access)')) 
         if (require) 
               require("devtools", character.only = TRUE)		
   	
	     if (!(is.character(substitute(Package.Name)))) 
             Package.Name <- deparse(substitute(Package.Name))
		 if(Package.Name == "NULL")
               Package.Name <- get.subs(Package, "/")[[2]]
		 devtools::install_github(Package, quiet = quiet, force = force)	  
		 if (!any(installed.packages()[, 1] %in% Package.Name))
	               stop(paste("GitHub", Package.Name, 'package is not installed; an attempt to install failed (check for internet access)'))  
		 if (require) 
             require(Package.Name, character.only = TRUE)		   
    } else {
	    if (any(installed.packages()[, 1] %in% Package)) {
		    update.packages(Package, ask = FALSE)
		}  else 
		    install.packages(Package, quiet = quiet)
		if (!any(installed.packages()[, 1] %in% Package))
	               stop(paste("CRAN", Package, 'package is not installed; an attempt to install failed (check for internet access)')) 
        if (require) 
           require(Package, character.only = TRUE)				   
	}
}
 
