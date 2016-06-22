se <- function (x, na.rm = FALSE)  {
   sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm = na.rm)/length(x[!is.na(x)] - 1))

}
