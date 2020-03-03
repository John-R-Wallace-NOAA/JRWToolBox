

switchSlash <- function (backSlash = readClipboard()) 
{
   forwardSlash <- gsub("\\\\", "/", backSlash)
   cat(forwardSlash, file = "clipboard")
   forwardSlash
}

