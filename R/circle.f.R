
circle.f <- function (cx, cy, r, fill.col = NULL, border.col = ifelse(is.null(fill.col), 'black', fill.col), 
       yAxis = TRUE, facets.num = 40, lty.neg = 2, lwd = ifelse(is.null(fill.col), 0, 2), ...) {
       
    if (!is.finite(r) | r == 0) 
        return(invisible())
        
    if (r < 0) 
        neg.flag <- T
    else neg.flag <- F
    r <- abs(r)
    
    z <- (seq(0, 360, 360/facets.num) * pi)/180
    pin <- par()$pin
    usr <- par()$usr
    adj <- (pin[2]/pin[1])/((usr[4] - usr[3])/(usr[2] - usr[1]))
    
    if (yAxis) {
        x <- sin(z) * r * adj + cx
        y <- cos(z) * r + cy
    }
    else {
        x <- sin(z) * r + cx
        y <- (cos(z) * r * 1)/adj + cy
    }
    
    if (is.null(fill.col)) {
        if (neg.flag) 
            lines(x, y, lwd = lwd, lty = lty.neg, ...)
        else lines(x, y, lwd = lwd, col = border.col, ...)
    }
    else {
        polygon(x, y, lty = 1, lwd = lwd, border = NA, col = fill.col, 
            ...)
        lines(x, y, lty = 1, lwd = lwd, col = border.col)
        if (neg.flag) 
            circle.f(cx, cy, r * 0.5, fill.col = 0, yAxis = yAxis, 
                facets.num = facets.num)
    }
    
    invisible({
        if (neg.flag) data.frame(x = cx, y = cy) else data.frame(x = x, y = y)
    })
}
