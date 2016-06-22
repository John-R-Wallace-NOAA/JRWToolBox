bar <-
function(i, n, size = 60, char = ">", prefix = memory.size())
{
        num <- round((size * i)/n)
        catf(prefix, " |", paste(rep(char, num), collapse = ""), paste(rep(" ", size - num), collapse = ""), "|\r", sep = "")
        invisible()
}

