
ptInsideWhichPolygon <- function(pt, polygons, test01 = TRUE) {
  lib(sp, require = FALSE)
  Out <- lapply(polygons, function(Poly) sp::point.in.polygon(pt$x, pt$y, Poly$x, Poly$y))
  names(Out[Out == TRUE])
}
