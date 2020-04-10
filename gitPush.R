
gitPush <- function(..., Dir) {
   
   # Clone repo using git
   shell("echo rem' > run.bat")
   shell("echo git config --global user.name 'John Wallace' >> run.bat")
   shell("echo git config --global user.email 'soundbirds@gmail.com'  >> run.bat")
   shell(paste0("echo git clone ", Dir, " >> run.bat"))
   shell("echo exit >> run.bat")
   shell("start run.bat")
   Sys.sleep(20)  # Default is for shell() to wait until process is done, but this still needs the extra time
   shell("del run.bat")
   Sys.sleep(5)
   
   # Save source files to the repo directory
   setwd(Dir)
   objList <- list(...)
   objNames <- paste0(as.character(substitute(list(...)))[-1], ".R")
   for(i in 1:length(objList) {
     capture.output(objList[i], file = objNames[i])
   }
   
   # Add files to cloned repo with git
   shell("echo rem' > run2.bat")
   for (i  in length(objNames)) {
     shell(paste0("echo git add ", objNames[i], " >> run2.bat")) 
   }
   
   # Add index, commit, and push files back to Github repo (only those files changed will be pushed up).
   shell("echo git add index.htm" >> run2.bat") 
   # shell("echo git commit -m "Single file push" >> run2.bat") 
   shell("echo git commit --amend --no-edit" >> run2.bat")     
   shell("echo git push -u -v --force origin master >> run2.bat")   
   shell("echo exit >> run2.bat")
   shell("start run2.bat")
   Sys.sleep(5)  
   shell("del run2.bat") 
   
   # Move back up to original working directory and remove repo directory
   setwd("..")
   system(paste0("rm -r ", Dir))
      
}
