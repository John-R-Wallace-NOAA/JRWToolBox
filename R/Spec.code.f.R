Spec.code.f <- function (com.name, year = "052002", char = FALSE, firstEntryOnly = FALSE) 
{
    Spec.code.name <- paste("Spec.code.", year, sep = "")
    Spec.code <- eval(parse(text = Spec.code.name))
    if (!char) {
        if (is.name(substitute(com.name))) 
            com.name <- deparse(substitute(com.name))
    }
    out <- Spec.code[grep(paste(com.name, sep = ""), Spec.code$CommonName), 
        ]
    if (nrow(out) == 0) {
        if (length(get.subs(com.name, sep = " ")) == 2) {
            tmp <- get.subs(com.name, sep = " ")
            com.name <- paste(casefold.f(tmp[1]), casefold.f(tmp[2], 
                FALSE), sep = " ", collapse = " ")
            out <- Spec.code[grep(paste(com.name, sep = ""), 
                Spec.code$CommonName), ]
        }
        else out <- Spec.code[grep(paste(casefold.f(com.name), 
            sep = ""), Spec.code$CommonName), ]
    }
    out2 <- out
    cat("\n")
    out <- Spec.code[grep(paste(com.name, sep = ""), Spec.code$ScientificName), 
        ]
    if (nrow(out) == 0) {
        if (length(get.subs(com.name, sep = " ")) == 2) {
            tmp <- get.subs(com.name, sep = " ")
            com.name <- paste(casefold.f(tmp[1]), tmp[2], sep = " ", 
                collapse = " ")
            out <- Spec.code[grep(paste(com.name, sep = ""), 
                Spec.code$ScientificName), ]
        }
        else out <- Spec.code[grep(paste(casefold.f(com.name), 
            sep = ""), Spec.code$ScientificName), ]
    }
    if(firstEntryOnly)
        sort.f(rbind(out2, out))[1, ]
    else
        sort.f(rbind(out2, out))    
    
}
