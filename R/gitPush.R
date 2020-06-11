
gitPush <- function(..., list = character(), gitDir, gitUserName = gitName, gitUserEmail = gitEmail, deleteRepoAfterPush = TRUE, verbose = FALSE)  {

    # Initial setup - the oddity of calling a character vector 'list' keeped from the 'rm' function code.
    
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
        is.character(x), NA, USE.NAMES = FALSE))) 
        stop("... must contain names or character strings")
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) 
        names <- character()
        
        
    list <- .Primitive("c")(list, names)
    
    
    HomeDir <- paste0(getwd(), "/")
    repo <- JRWToolBox::get.subs(gitDir, "/")[2] # Avoid the list produced by strsplit() [I wrote get.subs() in SPlus before strsplit() came out.] 
    if(dir.exists(repo)) {
    
       cat(paste0("\n\nThe directory: ", repo, " will be removed.\n"))
       switch(menu(c("Stop?", "Delete the directory and continue?")) + 1,
           stop("Stopped by user."), stop("Stopped by user."), cat("\n\n"))
    }
   
    system(paste0("rm -r -f ", repo)) # Make sure the repo directory is deleted 
    
    # Download function and scripts from GitHub (you will be asked once for your password, if you are not already logged into GitHub).
    JRWToolBox::git(paste0("config --global user.name '", gitUserName, "'"))
    JRWToolBox::git(paste0("config --global user.email '", gitUserEmail, "'"))
    JRWToolBox::git(paste0("clone https://github.com/", gitDir, ".git"))
    
    if(verbose) {
       cat("Files cloned from ", repo, ":\n", sep = "")
       print(list.files(repo))
    }
    
    # Copy the files to the local repo, add the files to the repo, and push the repo (only files that are changed are moved, that's how git push works).
    
    for (i in list)  {
      file.copy(i, repo, overwrite = TRUE)
      if(verbose)
         cat("\n", i, "was copied from", HomeDir, "to", repo, "\n")
    }
    
    setwd(paste0(HomeDir, repo))
    if(verbose) 
        cat("\n Working directory is now:", getwd(), "\n")
    
    for (i in list)  {
    
      JRWToolBox::git(paste0('add ', i))
      if(verbose)
         cat("\n", i, "was added to the local repo.\n")
    }
    
    JRWToolBox::git('commit --amend --no-edit --allow-empty')  
    JRWToolBox::git('push -u -v --force origin master')
    if(verbose)
       cat(paste0("\n The local copy of ", repo, " has been pushed to GitHub.\n"))
    
    setwd(HomeDir)
    if(verbose) 
        cat("\n Working directory is now:", getwd(), "\n")
    
    if(deleteRepoAfterPush) {
       system(paste0("rm -r -f ", repo))
       if(verbose) 
          cat("\n The local", repo, "directory was deleted.\n")   
    }
    
    invisible() 
}



