
dev.con <- function() {
  Temp <- tempfile('Dummy', fileext = '.png')
  png(Temp)
  dev.off()
  file.remove(Temp)
  invisible()
}

