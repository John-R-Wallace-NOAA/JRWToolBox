play.movie <- function (objs.list, sec = 0.3, forth.and.back = F, repeat.) 
{
    N <- length(objs.list)
    if (forth.and.back) 
        Order <- c(1:N, N:1)
    else Order <- 1:N
    if (!missing(repeat.)) 
        Order <- rep(Order, repeat.)
    for (i in 1:length(Order)) {
        replayPlot(objs.list[[Order[i]]])
        Sys.sleep(sec)
    }
    invisible()
}
