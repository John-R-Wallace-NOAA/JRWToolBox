Spec.code.FRAM.f <- function (com.name, char = F) {

    if (!char) {
        if (is.name(substitute(com.name))) 
            com.name <- deparse(substitute(com.name))
    }

    out <- SPECIES[grep(paste(com.name, sep = ""), SPECIES$NW_COMMONNAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]

    if (nrow(out) == 0) {
        if (length(get.subs(com.name, sep = " ")) == 2) {
            tmp <- get.subs(com.name, sep = " ")
            com.name <- paste(casefold.f(tmp[1]), tmp[2], sep = " ", 
                collapse = " ")
            out <- SPECIES[grep(paste(com.name, sep = ""), 
                SPECIES$NW_COMMONNAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]
        }
        else out <- SPECIES[grep(paste(casefold.f(com.name), 
            sep = ""), SPECIES$NW_COMMONNAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]
    }

    out2 <- out
    cat("\n")
    out <- SPECIES[grep(paste(com.name, sep = ""), SPECIES$NW_SCINAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]

    if (nrow(out) == 0) {
        if (length(get.subs(com.name, sep = " ")) == 2) {
            tmp <- get.subs(com.name, sep = " ")
            com.name <- paste(casefold.f(tmp[1]), tmp[2], sep = " ", 
                collapse = " ")
            out <- SPECIES[grep(paste(com.name, sep = ""), SPECIES$NW_SCINAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]
        }
        else out <- SPECIES[grep(paste(casefold.f(com.name), 
            sep = ""), SPECIES$NW_SCINAME), c("NW_SPECIESCODE", "NW_SCINAME", "NW_COMMONNAME")]
    }

    sort.f(rbind(out2, out))
}
