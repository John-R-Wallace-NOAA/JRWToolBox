
local_and_remote_SHA <- function(repo,
                           ref = "HEAD",
                           subdir = NULL,
                           auth_token = remotes:::github_pat(quiet),
                           host = "api.github.com",
                           dependencies = NA,
                           upgrade = c("default", "ask", "always", "never"),
                           force = FALSE,
                           quiet = FALSE,
                           build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                           build_manual = FALSE, build_vignettes = FALSE,
                           repos = getOption("repos"),
                           type = getOption("pkgType"),
                           ...) {
    # require(remotes)                       
     
    remote <- remotes:::github_remote(repo, ref = ref, subdir = subdir, auth_token = auth_token, host = host, ...)

    stopifnot(remotes:::is.remote(remote))
    package_name <- remotes:::remote_package_name(remote)
    local_sha <- remotes:::local_sha(package_name)
    remote_sha <- remotes:::remote_sha(remote, local_sha)
    c(local_sha = local_sha , remote_sha = remote_sha)
}


if(FALSE)  {

   SHAs <- local_and_remote_SHA( "John-R-Wallace-NOAA/rgit")
   
   SHAs
    
   SHAs[1] == SHAs[2] 

}


