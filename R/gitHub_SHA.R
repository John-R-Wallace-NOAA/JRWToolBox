gitHub_SHA <- function(repo, ref = "master") {

   devtools:::remote_sha(lapply(repo, devtools:::github_remote, username = NULL, 
        ref = "master", subdir = NULL, auth_token = devtools:::github_pat(quiet), 
        host = "https://api.github.com")[[1]])
}

		
