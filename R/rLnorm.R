rLnorm <- function(n, m, sd, verbose = FALSE) {

    '  # https://stackoverflow.com/questions/56821688/sample-a-lognormal-distribution-to-an-exact-mean-and-sd  '

   if(verbose)
     cat(paste0('\nmeanlog = ', log(m^2 / sqrt(sd^2 + m^2)), '\n\nsdlog = ', sqrt(log(1 + (sd^2 / m^2))), "\n\n"))
       
   scale(rlnorm(n, meanlog = log(m^2 / sqrt(sd^2 + m^2)), sdlog = sqrt(log(1 + (sd^2 / m^2))))) * sd + m
}
