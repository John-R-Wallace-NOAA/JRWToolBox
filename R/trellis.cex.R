trellis.cex <- function (cex = 1, sub.cex = 0.8) 
{
    trellis.par.set("add.text", list(cex = cex))
    trellis.par.set("box.dot", list(cex = cex))
    trellis.par.set("dot.symbol", list(cex = cex))
    trellis.par.set("plot.symbol", list(cex = cex))
    trellis.par.set("superpose.symbol", list(cex = cex * sub.cex))
    trellis.par.set("axis.text", list(cex = cex * sub.cex))
    trellis.par.set("par.xlab.text", list(cex = cex))
    trellis.par.set("par.ylab.text", list(cex = cex))
    trellis.par.set("par.main.text", list(cex = cex))
    trellis.par.set("par.sub.text", list(cex = cex))
}
