git.sf1 <- function(run = TRUE) {
 sf.name <- gitAFile("https://raw.githubusercontent.com/John-R-Wallace/Scratch/master/sf1.R")
  if(run)
    eval(parse(text = sf.name))()
}
