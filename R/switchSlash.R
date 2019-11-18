

switchSlash <- function (backSlash = readClipboard()) 
{
    forwardSlash <- gsub("\\\\", "/", backSlash)
    JRWToolBox::write.clip(forwardSlash)
    forwardSlash 
}

