git.sf2 <- function(run = TRUE) {
  sf.name <- gitAFile("https://raw.githubusercontent.com/John-R-Wallace/Scratch/master/sf2.R")
  if(run)
    eval(parse(text = sf.name))()
}
