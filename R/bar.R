bar <- function (i, n, size = 60, skipNum = ifelse(round(n/size) < 1, 1, round(n/size)), char = ">", OuterIndex = 1, prefix = ifelse(.Platform$OS.type == 
          "windows", paste(OuterIndex, ":", memory.size()), paste(OuterIndex, ":", i))) 
{
    if(i %% skipNum == 0) {
        num <- round((size * i)/n)
        cat(prefix, " |", paste(rep(char, num), collapse = ""), paste(rep(" ", 
            size - num), collapse = ""), "|\r", sep = "")
        flush.console()
    }    
    if (i == n) 
        cat("\n\r")
}
