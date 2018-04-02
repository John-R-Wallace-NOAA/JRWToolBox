
selectPts <- function (pts, list.of.lists.obj = 1, outside.poly = FALSE, col = "blue", 
    alpha = 0.5, lty = 1, ...) 
{
    lib(sp)
    if (is.list(pts) == TRUE & !is.null(names(pts[[list.of.lists.obj]]))) {
        if (names(pts[[list.of.lists.obj]])[1] == "ll") 
            pts <- pts[[list.of.lists.obj]]$llHIst
    }
    pts <- pts
    col <- col.alpha(col, alpha)
    Poly <- draw.polygon(col = col, lty = lty)
    tf <- as.logical(sp::point.in.polygon(pts[,1], pts[,2], Poly[,1], Poly[,2]))
    points(pts[tf, 1], pts[tf, 2], ...)
    if (!any(tf, na.rm = TRUE)) 
        stop("No points selected. A smoother polygon may be needed.")
    if (outside.poly) 
        pts[!tf, ]
    else pts[tf, ]
}
