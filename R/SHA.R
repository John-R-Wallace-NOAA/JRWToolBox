SHA <- function (repo) 
{
    SHA <- packageDescription(repo)$RemoteSha
    if (is.null(SHA)) 
        stop("No SHA number. Perhaps a CRAN package, not one downloaded from GitHub.")
    cat("\nSHA:", SHA)
    cat("\n\nCurrent call to revert to this Commit in the future with build date and time:\n\n", 
        "devtools::install_github('", packageDescription(repo)$RemoteUsername, "/", packageDescription(repo)$GithubRepo, 
        "', ref = '", SHA, "')  # ", packageDescription(repo)$Built, "\n\n", sep = "")
    invisible(list(Build = packageDescription(repo)$Built, Call = paste("devtools::install_github('", 
        repo, "', ref = '", SHA, "')", sep = "")))
}
