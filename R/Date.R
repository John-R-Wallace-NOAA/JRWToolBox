
Date <- function (collapse  = "_") 
{
    paste(get.subs(sub("  ", " ", date()), " ")[c(3, 2, 5)], 
        collapse = collapse)
}
