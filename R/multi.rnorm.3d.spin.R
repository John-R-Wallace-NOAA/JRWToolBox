
multi.rnorm.3d.spin <- 
function(corr = 0.75) {

  require(glots)
  require(aplpack)

  tmp <- hist2d(rmulti.norm(50000, dim=2, 0, 2, corr), nbins=50)
  spin3R(as.matrix(cbind(expand.grid(tmp$x, tmp$y), c(tmp$counts))))

}



