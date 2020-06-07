
pushFiles <- function(File, gitDir, gitUserName. = gitUserName, gitUserEmail. = gitUserEmail, deleteRepoAfterPush = TRUE, verbose = FALSE)  {

    # Initial setup 
    
    if (!(is.character(substitute(File)))) {
        File <- deparse(substitute(File))
    }
    
    HomeDir <- paste0(getwd(), "/")
    repo <- JRWToolBox::get.subs(gitDir, "/")[2] # Avoid the list produced by strsplit() [I wrote get.subs() in SPlus before strsplit() came out.] 
    if(dir.exists(repo)) {
    
       cat(paste0("\n\nThe directory: ", repo, " will be removed.\n"))
       switch(menu(c("Stop?", "Delete the directory and continue?")) + 1,
           stop("Stopped by user."), stop("Stopped by user."), cat("\n\n"))
    }
   
    system(paste0("rm -r -f ", repo)) # Make sure the repo directory is deleted 
    
    # Download function and scripts from GitHub (you will be asked once for your password, if you are not already logged into GitHub).
    JRWToolBox::git(paste0("config --global user.name '", gitUserName., "'"))
    JRWToolBox::git(paste0("config --global user.email '", gitUserEmail., "'"))
    JRWToolBox::git(paste0("clone https://github.com/", gitDir, ".git"))
    
    if(verbose) {
       cat("\nFiles cloned from ", repo, ":\n")
       print(list.files(repo))
    }
    
    # Copy the files to the local repo, add the files to the repo, and push the repo (only files that are changed are moved, that's how git push works).
    
    file.copy(File, repo, overwrite = TRUE)
    if(verbose)
       cat("\n", File, "was copied from", HomeDir, "to", repo, "\n")
    
    setwd(paste0(HomeDir, repo))
    if(verbose) 
        cat("\n Working directory is now:", getwd(), "\n")
        
    # write(File, test = paste0(
    JRWToolBox::git(paste0('add ', File))
    if(verbose)
       cat("\n", File, "was added to the local repo.\n")
    
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

