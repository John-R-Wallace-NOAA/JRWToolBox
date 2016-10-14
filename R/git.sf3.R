git.sf3 <- function(run = TRUE) {
  sf.name <- gitAFile("https://raw.githubusercontent.com/John-R-Wallace/Scratch/master/sf3.R")
  if(run)
    eval(parse(text = sf.name))()
}
