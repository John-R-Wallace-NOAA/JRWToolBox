
Windows <- function() {
  if(names(dev.cur()) == "windows") eval(parse(text = names(dev.cur())))()
}
