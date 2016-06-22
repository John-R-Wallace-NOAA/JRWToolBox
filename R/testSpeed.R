testSpeed <- function( N = 2000) {

   m <- matrix(rlnorm(N*N), ncol=N)

   system.time(solve(m%*%m%*%m))
}
