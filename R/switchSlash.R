

switchSlash <- function (backSlash = readClipboard()) 
{
    forwardSlash <- gsub("//", "/", gsub("\\\\", "/", backSlash))
    cat(forwardSlash, file = "clipboard")
    forwardSlash
}

sS <- function (backSlash = readClipboard()) 
{
    forwardSlash <- gsub("//", "/", gsub("\\\\", "/", backSlash))
    cat(forwardSlash, file = "clipboard")
    forwardSlash
}





