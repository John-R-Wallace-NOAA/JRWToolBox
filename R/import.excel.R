import.excel <- function (PATH, ...) 
{
    tmp <- get.subs(.Path, "/")
    read.xls(print(paste(c(tmp[1:(length(tmp))], PATH), collapse = "\\")), 
        ..., perl = "W:\\perl\\bin\\perl.exe")
}
