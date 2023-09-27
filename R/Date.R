
Date <- function (time = FALSE, collapse  = "_") {

    dateSubs <- get.subs(sub("  ", " ", date()), " ")
    
    if(time) {
       timeSubs <- gsub(":", collapse, dateSubs[4])
       paste0(paste(dateSubs[c(3, 2, 5)], collapse = collapse), collapse, timeSubs)
    } else
       paste(dateSubs[c(3, 2, 5)], collapse = collapse)
}
