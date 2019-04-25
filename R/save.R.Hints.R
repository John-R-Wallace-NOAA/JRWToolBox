save.R.Hints <- function () {

# Split "..." among two functions

est <- function(a,bb, ...) {

  all.dots <- list(...)
  all.dots.names <- names(all.dots)
  print(all.dots[grepl("plt.", names(all.dots))])
  cat("=========\n\n")
  all.dots[!grepl("plt.", names(all.dots))] 

}


# First put "all.dots <- list(...)" on top of function.
# Then use i.e.,  do.call(FCN, c(list, all.dots[grepl("plt.", names(all.dots))]))




"Read file from upto Splus 2000. Name needs to be DOS 8.3"

read.S("C:\\all_usr\\jrw\\bathymet\\_Data\\env")



"May have to use 'start' with shell()"

shell("start xcopy /I c:\\ALL_USR\\jrw\\bathymet.r\\*.txt c:\\ALL_USR\\jrw\\bathymet.r\\tmp\\")

"'&&' may not always work with start"



"Best way to run multiple commands in DOS"

shell("echo cd c:\\ALL_USR\\jrw\\bathymet.r\\.Data > run.bat")
shell("echo xcopy /I ..\\*.txt ..\\tmp\\  >> run.bat")
shell("echo exit >> run.bat")
shell("start run.bat")
shell("del run.bat")



"Retrieve an R Object, Including from a Namespace"

getAnywhere("trueRandom")

#=========From Splus===============================

    # *** Name to character *** 
        
        # For a single object
        
        name.object <- deparse(substitute(object))
        
        # For multiple objects, from rm()
        
        names <- as.character(substitute(list(...)))[-1]
        

   # *** Character to name *** 
        
        tmp <- c(12, 23, 10)
        tmp2 <- "tmp"
        mean(eval(parse(text = tmp2)))
          
        # *** Use of nf() [NA to F] *** 
        # nf() enables one to avoid evaluating the conditional
        # twice when one is forced to explicitly avoid NA's
        
        xx <- data.frame(ss = c(NA, 2:5), tt = 2:6)
        xx[xx$ss <= 4 & !is.na(xx$ss <= 4), 1] <- 99
        
        xx[nf(xx$ss <= 4), 1] <- 99


# =======================================================

# For trellis plot with confidence intervals use xYplot()

# To change all cex in trellis use trellis.cex() 
# See W:\ALL_USR\JRW\Assessment\Yelloweye\YE2009\Carey_Yelloweye_Mtg_5_20\Triennial YEY off WA, GLMM vs. GLM coef.R

# To copy a figure to png use png.copy().  Is not WYSWYG!


# =======================================================
# Grid.locator {grid}   R Documentation
# Capture a Mouse Click
# Description

# Allows the user to click the mouse once within the current graphics device and returns the location of the
#    mouse click within the current viewport, in the specified coordinate system. 

# =======================================================


# To see objects inside a namespace

base::ls(loadNamespace("mcmc"))

# =======================================================

# Matching and pushing out functions and data to e.g. load into GitHub

tmp <- match.f(LL(4), LL(2), 1, 1, 1)
names(tmp)[4] <- "Match"
tmp <- renum(tmp)
names(tmp)[1] <- "Names"

tmp <- renum(tmp[tmp$Class %in% "function",])

for ( i in 1:nrow(tmp)) {

     TMP <- get(tmp$Names[i], pos = 4)
     attr(TMP, "source") <- NULL
     sink(paste("ToolBox/", tmp$Names[i], ".R", sep=""))
     cat(paste(tmp$Names[i], "<- "))
     print(TMP)
     sink()
}

dump("nw.poly", file=paste("ToolBox/nw.poly.R"))
dump("Spec.code.052002", file=paste("ToolBox/Spec.code.052002.R"))






}



