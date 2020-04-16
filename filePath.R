 
filePath <- function(oldPath = readClipboard()) 
{
    '  # filePath takes path from the clipboard and replaces it with the machine dependent correct path, which is also output  '
    correctPath <- do.call(file.path, as.list(strsplit(gsub("\\\\", " ", oldPath), " ")[[1]]))
    cat(correctPath, file = "clipboard")
    correctPath
}

