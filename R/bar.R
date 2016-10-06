bar <- function (i, n, size = 60, char = ">", OuterIndex = 1, prefix = ifelse(.Platform$OS.type == 
                 "windows", paste(OuterIndex, ":", memory.size()),  paste(OuterIndex, ":", i))) 
{
    num <- round((size * i)/n)
    cat(prefix, " |", paste(rep(char, num), collapse = ""), paste(rep(" ", 
        size - num), collapse = ""), "|\r", sep = "")
    flush.console()
    if (i == n) 
        cat("\n\r")
}
