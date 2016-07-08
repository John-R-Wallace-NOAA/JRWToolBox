showObject <- function (x, fileName = tempfile(), digits = 1, row = F, col = F, sep = "\t", quote = F, pager = "winvi.exe", ...) {

    file.create(fileName)

    if (all(apply(x, 2, is.numeric))) 
        write.table(round(x, digits), fileName, row = row, col = col, 
            sep = sep, quote = quote, ...)
    else write.table(x, fileName, row = row, col = col, sep = sep, 
        quote = quote, ...)
    file.show(fileName, pager = pager)
}

