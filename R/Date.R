
Date <- function (Time = FALSE, collapse  = "_") {
    
    '  # Note: To get the date correct, 2 spaces down to 1 space is needed when there is a single digit day of month  ' 
    dateSubs <- get.subs(sub("  ", " ", date()), " ")
    
    if(Time) {
       timeSubs <- gsub(":", collapse, dateSubs[4])
       paste0(paste(dateSubs[c(3, 2, 5)], collapse = collapse), collapse, timeSubs)
    } else
       paste(dateSubs[c(3, 2, 5)], collapse = collapse)
}
