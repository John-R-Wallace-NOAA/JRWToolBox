
Log.Norm.CI.Cox.t <- function(x = NULL, MeanLogNorm = NULL, SDLogNorm.Pop = NULL, N = NULL, BootReps = 10000, Prob = 0.95) {

'  # Confidence intervals for the mean of a Log-Normal Distribution using the Cox method modified to use a t distribution is from:  '
"  #         Journal of Statistics Education Volume 13, Number 1 (2005), www.amstat.org/publications/jse/v13n1/olsson.html  "
"  #         In the paper a simulation study was performed to compare this method with others.  "

"   # This function, including the parametric bootstrapping for the case when only the mean, SD, and N of the log-normal data are known, is by:  "
"   #        John Wallace John.Wallace@noaa.gov  "

"   # Note that if only the SD of the mean is available, then the argument 'SDLogNorm.Pop' could be set to SD * N .  "
"   # Note also, that if the exact value of N is not known, a reasonably close estimate of N will still give a good estimate of the CI.  "

 if(!is.null(x)) {   # Apply the modified Cox function to log-normal data

    N <- length(x)
    MeanLogNorm <- mean(x)
    Var <- var(log(x))
 
    c(exp(mean(log(x)) + Var/2 - qt(1-(1-Prob)/2, N) * sqrt(Var/N + Var^2/(2*(N-1)))), MeanLogNorm,
        exp(mean(log(x)) + Var/2 + qt(1-(1-Prob)/2, N) *sqrt(Var/N + Var^2/(2*(N-1)))))

 } else {    # Use the mean, SD, and N from log-normal data and do a parametric bootstrap 

    SDNorm <- sqrt(log((SDLogNorm.Pop/MeanLogNorm)^2+1)) # SD in normal space, since CV = sqrt(exp(sigma^2) - 1) for a log-normal
    MeanNorm <- log(MeanLogNorm) - SDNorm^2/2  # Mean in normal space using correction

    x <- rlnorm(N * BootReps, MeanNorm, SDNorm) # Create random log-normals in a matrix of size N X BootReps
    x.mat <- matrix(x, ncol = BootReps)

    Boots <- apply(x.mat, 2, function(x) {  # Apply the modified Cox function to each column

      Var <- var(log(x))
      tmp <- qt(1-(1-Prob)/2, N) * sqrt(Var/N + Var^2/(2*(N-1)))
      c(exp(mean(log(x)) + Var/2 - tmp),
        exp(mean(log(x)) + Var/2 + tmp))
     })

 out <- apply(Boots, 1, mean)   # Take the average by row
 c(out[1], MeanLogNorm, out[2]) # Output answer

 }
}
