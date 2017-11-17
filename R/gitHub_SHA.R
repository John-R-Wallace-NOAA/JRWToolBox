gitHub_SHA <- function (repo, ref = "master") 
{
    SHA <- devtools:::remote_sha(lapply(repo, devtools:::github_remote, 
        username = NULL, ref = "master", subdir = NULL, auth_token = devtools:::github_pat(quiet), 
        host = "https://api.github.com")[[1]])
    cat("\nSHA:", SHA, "from reference:", ref)
    cat("\n\nCurrent date and time and the call to reinstall this Commit in the future:\n\n", as.character(Sys.time()), "\ndevtools::install_github('", repo, "', ref = '", SHA, "')\n\n", sep="")
}
	
