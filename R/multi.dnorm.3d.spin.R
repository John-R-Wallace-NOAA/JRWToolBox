
multi.dnorm.3d.spin <- function(len=40) { 

  require(aplpack)

  tmp <- seq(-4, 4, len=len)
  tmp2 <- expand.grid(tmp, tmp)
  spin3R(as.matrix(cbind(tmp2, apply(tmp2, 1, dmulti.norm))))

}




