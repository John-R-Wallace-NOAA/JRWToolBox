
lsNs <- function(packageOrPathNumber) {

    if (!(is.character(substitute(packageOrPathNumber)))) {
        packageOrPathNumber <- deparse(substitute(packageOrPathNumber))
    }

    oldOpts <- options(warn = -1)
    on.exit(options(oldOpts))
    packageOrPathNumber <- ifelse(is.na(as.numeric(packageOrPathNumber)), packageOrPathNumber, as.numeric(packageOrPathNumber))
 
   if(is.numeric(packageOrPathNumber))
      print(base::ls(getNamespace(JRWToolBox::get.subs(search()[packageOrPathNumber], ':')[2]), all.names = TRUE))
     
   if(is.character(packageOrPathNumber))
       print(base::ls(getNamespace(packageOrPathNumber), all.names=TRUE))
       
}       

