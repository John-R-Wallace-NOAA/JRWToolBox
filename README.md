<h3> R-ToolBox </h3>

---

Prologue

Any csv file, script (of any kind), or R function and pdf on GitHub, including those in this toolbox can be individually downloaded using the function gitAFile() found in this toolbox. (Which could initially be copied from the 'R' directory here and pasted into R.)

For example use:

    gitAFile('John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R') 
    # lib() is saved to .GlobalEnv and also scrolled on the screen since show = TRUE (the default)
    # Use the Raw view URL as a guide to the correct path, but note that in the URL a space ' ' codes as '%20' 
    # In a file name '%20's can be left as spaces (they will be changed for you).

to view and download lib() into R. (The lib() function will install and update both GitHub and CRAN packages only if needed and regardless will load the package into the R session, if require = TRUE (the default).)

Note that the FUNCTION DOWNLOADED WILL HAVE ALL THE COMMENTS AND LINE SPACING INTACT, unlike functions within packages downloaded via the 'devtools' package (which, FYI, now calls the 'remotes' package).

See additional arguments of gitAFile() with str(gitAFile).

Customized wrappers for gitAFile can be made for often used packages, e.g.:
       
       S <- function (File, ...) {
            if (!(is.character(substitute(File)))) 
                File <- deparse(substitute(File))
            gitAFile(paste0("John-R-Wallace-NOAA/JRWToolBox/master/R/", File, ".R"), ...)
       }
       
so that only:

      S(gitAFile)
 
 is needed to download and view gitAFile().
 
 An example to directly display the pdf fully in a viewer and not on the GitHub web site:
 
    gitAFile("https://github.com/James-Thorson/VAST/blob/master/manual/VAST_model_structure.pdf", "pdf")
    
To save a script (not an R function) as a file in the current working directory do, e.g.:

     gitAFile("John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/Run_Data_and_VAST_by_Species.R", 'script', File = 'Run_Data_and_VAST_by_Species.R') 
     
 To download and put a function directly into an editor, copy and edit gitEdit()'s paths to a particular package and your favorite editor, e.g.:
 
      gitEdit(Table)
      
 downloads JRWToolBox::Table() and puts it into notepad++ for editing (if one of the two paths to notepad++ in gitEdit is correct).    

---


Install or upgrade this package with:

    # Get devtools if you don't already have it.
    if (!any(installed.packages()[, 1] %in% "devtools"))  install.packages('devtools')  
    
    remotes::install_github("John-R-Wallace-NOAA/JRWToolBox", INSTALL_opts = "--no-staged-install")
    
    # # Some R installations may require download.file.method = "auto":
    # oldOpts <- options(download.file.method = "auto")  # Sometimes remotes::install_github() throws an error without this
    # remotes::install_github("John-R-Wallace-NOAA/JRWToolBox", force = TRUE)
    # options(oldOpts)

If you then want to load the package into R use:

    library(JRWToolBox)    

----------------------

Highlights and Comments

- lib() function will install and update both GitHub and CRAN packages only if needed and regardless will load the package into the R session, if require = TRUE (the default).

Code to put on the top of a function to start using lib() and other functions in JRWToolBox would look like:

    if (!any(installed.packages()[, 1] %in% "devtools"))  
        install.packages('devtools')  
	
    if (!any(installed.packages()[, 1] %in% "JRWToolBox")) 
        devtools::install_github("John-R-Wallace/JRWToolBox")
    require(JRWToolBox)
    
    lib("John-R-Wallace/JRWToolBox")  # For keeping the toolbox updated
     
    # Install other packages as needed from either GitHub or CRAN
    lib("John-R-Wallace/Imap") # GitHub
    lib(plyr) # CRAN

- %ino% preserves the order when using matching operators unlike %in%.  See my entry in Stack Overflow:
https://stackoverflow.com/questions/10586652/r-preserve-order-when-using-matching-operators-in


- gitHub_SHA() shows the current (full) SHA for a given repo.  A call is also given that can be used to revert to that commit in the future. Saving these calls for each relevant package with your current code insures you are using the commits that your code works with and that your results are reproducible. The date and call are also invisibly returned and hence can be used to install the commit at any time in the future:
      
      JRWToolBox.Commit <- SHA("JRWToolBox")
      
      SHA: 278e298b805ef862065d8e5e733e96d54c1b3e9c
      
      Current call to revert to this Commit in the future with build date and time:
      
      devtools::install_github('John-R-Wallace/JRWToolBox', ref = '278e298b805ef862065d8e5e733e96d54c1b3e9c')  # R 3.5.0; ; 2018-08-10 21:53:30 UTC; windows
      
      
      JRWToolBox.Commit 
      $`Build`
      [1] "R 3.5.0; ; 2018-08-10 21:53:30 UTC; windows"
      
      $Call
      [1] "devtools::install_github('John-R-Wallace/JRWToolBox', ref = '278e298b805ef862065d8e5e733e96d54c1b3e9c')"
      
      eval(parse(text = JRWToolBox.Commit$Call))
      Downloading GitHub repo John-R-Wallace/JRWToolBox@278e298b805ef862065d8e5e733e96d54c1b3e9c ...
      ...
      
      packageDescription('JRWToolBox')$RemoteSha


- Ls() is a replacement for ls() to avoid the quotes on the object names. Setting the argument all=TRUE will list all object names in the search() path consistent with the search pattern given (find(..., simple=F) only gives back items in the search list, not the object names themselves).

- dir.use() is a wrapper function which creates the path of a sub-directory within a function call, if it doesn't already exist, and then returns the path: some.function(..., path = dir.use('c:/create_this_path_if_needed')

- showObject() shows an R object in an editor, edit the function to change the default editor for ease of use.

-------------------   
All functions were written by me except for (this list is under construction):

- xlsxToR: Substantial fix and value added changes; see the fork off of xlsxToR: https://gist.github.com/John-R-Wallace/3eab07a93877e87ec968
- mvnorm, dmulti.norm, Inside.polygon, inside.xypolygon, & verify.xypolygon: submissions to various Splus user group website(s)
- Some panel.XX functions are value added versions from the ones in the lattice package, however panel.chull, panel.chull.2.peels, & panel.conf.pred.band are mine.
- ll() is value added from a function by Arni Magnusson found in the package 'gdata'.
- Symbols is an old version of pchShow given in the help of the points function.
 
   

