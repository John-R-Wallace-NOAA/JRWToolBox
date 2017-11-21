
install_github_save_SHA <- function(repo, packageName = NULL, ref = 'master')  {

       devtools::install_github(repo,  ref = ref)
       currentSHA <- gitHub_SHA(repo, ref=ref)
	   if(is.null(packageName))
           save(currentSHA, file = file.path(R.home(), "library", strsplit(repo, "/")[[1]][2], "currentSHA.Rdata") )
	   else 
	       save(currentSHA, file = file.path(R.home(), "library", packageName, "currentSHA.Rdata") )
       invisible(currentSHA)
}
