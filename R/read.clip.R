read.clip <- function (vector = F, head = T, as.is = T, ...) 
{
    if (vector) 
        as.numeric(unlist(as.vector(read.table("clipboard", head = F))))
    else read.table("clipboard", head = head, as.is = as.is, 
        ...)
}
