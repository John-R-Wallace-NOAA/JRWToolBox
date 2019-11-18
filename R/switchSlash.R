

switchSlash <- function() {
    Out <- gsub("\\\\", "/", readClipboard())
    JRWToolBox::write.clip(Out)
    Out
}

