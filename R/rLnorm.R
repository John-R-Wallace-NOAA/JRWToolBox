rLnorm <- function(n, m, sd) {
    '  # https://stackoverflow.com/questions/56821688/sample-a-lognormal-distribution-to-an-exact-mean-and-sd  '
    m2 <- log(m^2 / sqrt(sd^2 + m^2))
    sd2 <- sqrt(log(1 + (sd^2 / m^2)))
    cat(paste0('\nmeanlog = ', m2, '\n\nsdlog = ', sd2, "\n\n"))
    r <- c(scale(rnorm(n)))
    scale(exp(sd2*r + m2), scale = FALSE) + m
}
