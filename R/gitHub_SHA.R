gitHub_SHA  <- function (repo, ref = "master", quiet = FALSE)  {
    SHA <- JRWToolBox::local_and_remote_SHA("John-R-Wallace-NOAA/JRWToolBox")[[2]]
    cat("\nSHA:", SHA, "from reference:", ref)
    cat("\n\nCurrent date and time and the call to revert to this Commit in the future:\n\n", 
        as.character(Sys.time()), "\nremotes::install_github('", 
        repo, "', ref = '", SHA, "')\n\n", sep = "")
    invisible(list(Date = Sys.time(), Call = paste("remotes::install_github('", 
        repo, "', ref = '", SHA, "')", sep = "")))
}
