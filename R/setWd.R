

setWd <- function()  {
  '  # See switchSlash() for only switching the slash in a path  '
  setwd(gsub  ( "\\\\",  "/",  readClipboard ()  ) )
  getwd()

}
