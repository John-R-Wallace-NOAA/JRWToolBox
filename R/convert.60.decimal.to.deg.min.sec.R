

convert.60.decimal.to.deg.min.sec <- function(x) {
 
   x2 <- floor(x)
   x3 <- (x - x2) * 60
   x4 <- floor(x3)
   x5 <- (x3 - x4) * 60
   x6 <- round(x5 <- (x3 - x4) * 60)
   paste(x2, x4, x6, sep = ":")
}
