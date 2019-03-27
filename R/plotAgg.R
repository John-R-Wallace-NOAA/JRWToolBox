
 
plotAgg <- function(x, y, ylab = paste('Average'), xlab= deparse(substitute(x)), main = deparse(substitute(y)), ylim = c(0, 1.025 * max(Agg$y, na.rm = TRUE)), type = 'o') {
    Agg <- aggregate(List(y), List(x), mean, na.rm = TRUE)
    plot(Agg$x, Agg$y, type = type, ylab = ylab, main = main, xlab= xlab, ylim = ylim)
    invisible(Agg)
}
