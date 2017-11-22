

install_github_save_SHA <- function(repo, ref = 'master')  {

       devtools::install_github(repo,  ref = ref)
       currentSHA <- JRWToolBox::gitHub_SHA(repo, ref=ref)
	   packageName <- devtools:::remote_package_name(lapply(repo, devtools:::github_remote, 
          username = NULL, ref = ref, subdir = NULL, auth_token = devtools:::github_pat(quiet), 
          host = "https://api.github.com")[[1]])		
	   save(currentSHA, file = file.path(R.home(), "library", packageName, "currentSHA.Rdata") )
       invisible(currentSHA)
}

