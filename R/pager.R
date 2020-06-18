
pager <- function(funcOut, pager. = options()$pager) {

  oldOP <- options(pager = pager.)
  on.exit(options(oldOP))
  
  tempFile <- tempfile()
  capture.output(funcOut, file = tempFile)
  file.show(tempFile)
}


