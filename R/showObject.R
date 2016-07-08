showObject <- function (x, digits = 1, row = F, col = F, sep = "\t", quote = F, ...) 
{
    tmpFile <- tempfile()
    file.create(tmpFile)
    
    if (all(apply(x,2,is.numeric))) 
        write.table(round(x, digits), tmpFile, row = row, 
            col = col, sep = sep, quote = quote, ...)
    else write.table(x, tmpFile, row = row, col = col, sep = sep, 
        quote = quote, ...)

    file.show(tmpFile, pager="winvi.exe")
}
