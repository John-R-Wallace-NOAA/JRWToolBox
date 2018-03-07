	 
SHA <- function (repo) 
{
    SHA <- packageDescription("multidplyr")$RemoteSha
    cat("\nSHA:", SHA)
    cat("\n\nCurrent date and time and the call to revert to this Commit in the future:\n\n", 
        as.character(Sys.time()), "\ndevtools::install_github('", 
        repo, "', ref = '", SHA, "')\n\n", sep = "")
    invisible(list(Date = Sys.time(), Call = paste("devtools::install_github('", 
        repo, "', ref = '", SHA, "')", sep = "")))
}
