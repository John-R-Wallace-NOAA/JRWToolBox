cumsd <- 
function(x) {

 N <- length(x)
 One.to.N.minus.1 <- c(1,1:(N-1))

 sqrt(cumsum(x^2)/One.to.N.minus.1 - (cumsum(x)^2)/((1:N)*One.to.N.minus.1))
}

