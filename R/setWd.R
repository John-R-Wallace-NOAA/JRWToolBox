

setWd <- function()  {

  setwd(gsub  ( "\\\\",  "/",  readClipboard ()  ) )
  getwd()

}
