panel.chull.2.peels <- 
  function (x, y, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, lwd = plot.line$lwd, lty = plot.line$lty, 
    col, col.line = plot.line$col, type, horizontal = FALSE, 
    ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    
    plot.line <- trellis.par.get("plot.line")
    
    panel.points(x[ok], y[ok])

    Pts <- cbind(x[ok], y[ok])
    hpts <- chull(Pts)
    hpts <- c(hpts, hpts[1])
    llines(Pts[hpts, ], col='red')

    Pts <- Pts[-hpts,]
    hpts <- chull(Pts)
    hpts <- c(hpts, hpts[1])
    llines(Pts[hpts, ], col='blue')
        
    
}



panel.chull <- 
  function (x, y, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, lwd = plot.line$lwd, lty = plot.line$lty, 
    col, col.line = plot.line$col, type, horizontal = FALSE, 
    ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    
    plot.line <- trellis.par.get("plot.line")
    
    panel.points(x[ok], y[ok])

    Pts <- cbind(x[ok], y[ok])
    hpts <- chull(Pts)
    hpts <- c(hpts, hpts[1])
    llines(Pts[hpts, ], col='red')
    
}

