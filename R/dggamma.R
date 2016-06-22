dggamma <- function(x, r, mu, beta) {

  beta * gamma(r)^-1 * mu^(beta*r) * x^(beta*r-1)*exp(-(mu*x)^beta)

}


