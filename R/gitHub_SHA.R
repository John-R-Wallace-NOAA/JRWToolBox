gitHub_SHA <- function (repo, ref = "master", quiet = FALSE)  {
    SHA <- remotes:::remote_sha(lapply(repo, remotes:::github_remote, 
        username = NULL, ref = ref, subdir = NULL, auth_token = remotes:::github_pat(quiet), 
        host = "https://api.github.com")[[1]])
    cat("\nSHA:", SHA, "from reference:", ref)
    cat("\n\nCurrent date and time and the call to revert to this Commit in the future:\n\n", 
        as.character(Sys.time()), "\nremotes::install_github('", 
        repo, "', ref = '", SHA, "')\n\n", sep = "")
    invisible(list(Date = Sys.time(), Call = paste("remotes::install_github('", 
        repo, "', ref = '", SHA, "')", sep = "")))
}


