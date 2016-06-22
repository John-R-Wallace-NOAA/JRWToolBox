
write.clip <- function (x, digits = 1, sep = "\t", quote = F, Round = T, ...) 
{
    require(MASS)
    if (all(apply(x,2,is.numeric)) & Round) 
        write.matrix(round(x, digits), "clipboard", sep = sep, ...)

    else write.matrix(x, "clipboard", sep = sep, ...)
}

if(F) {

write.clip.OLD <- function (x, digits = 1, row = F, col = T, sep = "\t", quote = F, Round = T, ...) 
{
    if (all(apply(x,2,is.numeric)) & Round) 
        write.table(round(x, digits), "clipboard", row = row, col = col, sep = sep, quote = quote, ...)

    else write.table(x, "clipboard", row = row, col = col, sep = sep, quote = quote, ...)
}

}
