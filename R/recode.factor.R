recode.factor <- function (x, new, old = levels(x)) 
as.factor(recode(as.character(x), new, old, nomatch = NA))
