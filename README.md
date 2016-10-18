R-ToolBox
============================

Prologue

Any csv file, script (of any kind), or R fucntion, including those in this toolbox can be individually downloaded using the function gitAFile() found in this toolbox. (Which could intially be copied from the 'R' directory here and pasted into R.)

For example to download just the lib() function which replaces the 2 stop process of install.packages('foo'); library(foo) with just one step:

    gitAFile('https://raw.githubusercontent.com/John-R-Wallace/R-ToolBox/master/R/lib.R')

Note the use of the raw GitHub, see: http://rawgit.com/

Install the entire package with:

    devtools::install_github("John-R-Wallace/R-ToolBox")

============================   

All functions were written by me except for (this list is under construction):

- xlsxToR: Substantial fix and value added changes; see the fork off of xlsxToR: https://gist.github.com/John-R-Wallace/3eab07a93877e87ec968
- mvnorm, dmulti.norm, Inside.polygon, inside.xypolygon, & verify.xypolygon: submissions to various Splus user group website(s)
- Some panel.XX functions are value added versions from the ones in the lattice package, however panel.chull, panel.chull.2.peels, & panel.conf.pred.band are mine.
- ll() is value added from a function by Arni Magnusson found in the package 'gdata'.
- Symbols is an old version of pchShow given in the help of the points function.
 
   
Comments on functions:

- Ls() is a replacement for ls() to avoid seeing all the quotes on the object names.
- dir.use() is a wrapper function which creates a path of sub-directory given, if it doesn't already exist and then returns the      path: some.function(..., path = dir.use('c:/create_this_path_if_needed')
- showObject() shows an R object in an editor, edit the function to change the default editor for ease of use.
