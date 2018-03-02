
rText <- function(RX, RY, ...) {
     '  # Relative text wrapper for text() '
     USR <- par()$usr
     text(RX * (USR[2] - USR[1]) + USR[1], RY * (USR[4] - USR[3]) + USR[3], ...)
}     
