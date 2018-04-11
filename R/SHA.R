SHA <- function (repo) 
{
    SHA <- packageDescription(repo)$RemoteSha
    if (is.null(SHA)) 
        stop("No SHA number. Perhaps a CRAN package, not one downloaded from GitHub.")
    cat("\nSHA:", SHA)
    cat("\n\nCurrent date and time and the call to revert to this Commit in the future:\n\n", 
        as.character(Sys.time()), "\ndevtools::install_github('", packageDescription(repo)$RemoteUsername, "/", 
		packageDescription(repo)$GithubRepo, "', ref = '", SHA, "')\n\n", sep = "")
    invisible(list(Date = Sys.time(), Call = paste("devtools::install_github('", 
        repo, "', ref = '", SHA, "')", sep = "")))
}
