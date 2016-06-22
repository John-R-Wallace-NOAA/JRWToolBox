plotCI <- function (x, y = NULL, ui = NULL, li = NULL, uiw, liw = uiw, ylo = NULL, yhi = NULL,...,
             sfrac = 0.01, ymax = NULL, add = FALSE, line.type = 'o', line.col = "black", int.col="black") {

    # Written by Venables; modified for access to ylim, contents, and color
    if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (is.null(y)) {
      if (is.null(x)) {
        stop("both x and y NULL")
      }
      y <- as.numeric(x)
      x <- seq(along = x)
    }

    if(is.null(ui))
        ui <- y + uiw
     if(is.null(li))
       li <- y - liw

    ylim <- range(c(y, ui, li, ylo, yhi, ymax))
    if (!add) plot(x, y, type='n', ylim = ylim, ...)
    segments(x, li, x, ui, col= int.col)
    smidge <- diff(par("usr")[1:2]) * sfrac
    x2 <- c(x, x)
    ul <- c(li, ui)
    segments(x2 - smidge, ul, x2 + smidge, ul, col= int.col)
    points(x, y, col= line.col, type = line.type, ...)
    invisible(list(x = x, y = y))
  }


