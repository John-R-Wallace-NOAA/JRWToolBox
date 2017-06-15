
write.clip <- function (x, digits = 1, sep = "\t", quote = F, Round = T, vectorToColumn = TRUE, ...) 
{
    require(MASS)
    if(is.null(dim(x)))
        if(vectorToColumn) x <- matrix(x, ncol = 1) else x <- matrix(x, nrow = 1)
    if (all(apply(x, 2, is.numeric)) & Round) 
        write.matrix(round(x, digits), "clipboard", sep = sep, 
            ...)
    else write.matrix(x, "clipboard", sep = sep, ...)
}



write.table.clip <- function (x, digits = 1, row = F, col = T, sep = "\t", quote = F, Round = T, ...) 
{
    if (all(apply(x,2,is.numeric)) & Round) 
        write.table(round(x, digits), "clipboard", row = row, col = col, sep = sep, quote = quote, ...)

    else write.table(x, "clipboard", row = row, col = col, sep = sep, quote = quote, ...)
}

