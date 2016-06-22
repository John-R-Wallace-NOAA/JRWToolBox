
CrossValid.Covar <- function(y, x, iter=100, ...) {

out <- NULL
  for ( i in 1:iter) {

    N <- length(x)

    S1 <- sample(1:N, floor(N/2))
    S2 <- sample(1:N, ceiling(N/2))

    y1 <- y[S1]
    x1 <- x[S1]

    F1 <- glm(y1 ~ x1, ...)
    Y2.HAT <- predict(F1, newdata = data.frame(x1 = x[S2]), type = "response")
   # Y2.HAT.2 <- F1$coef[1] +  F1$coef[2] * x[S2]

    F2 <- glm(Y2.HAT ~ x[S2], ...)

    out <- c(out, sum(abs(summary(F1)$cov.scaled - summary(F2)$cov.scaled)))


  }
  
  mean(out)
}




x <- seq(0, 100, len=1000)
y <- 3 + 4*x + rnorm(1000, sd=4)
CrossValid.Covar(y, x, 100)

y <- 3 + 4*x + rnorm(1000, sd=40)
CrossValid.Covar(y, x, 100)




L <- seq(5, 60, len=100)

P <- 1 -  1/(1 + exp(.3 + .1* (L + rnorm(100, sd=1))))

plot(L, P)

 Y <- cbind(round(P*1000000), rep(1000000, length(P)) - round(P*1000000))
# Y <- cbind(round(P*100), rep(100, length(P)) - round(P*100))

glm(Y ~L, family=binomial)
summary(glm(Y ~L, family=binomial))$cov.scaled


CrossValid.Covar(P, L, family=binomial, iter=500)

1           2            4            6           8 
0.008962203 0.00801388   0.007341321  0.008275168 0.007606578



N <- length(x)

    S1 <- sample(1:N, floor(N/2))
    S2 <- sample(1:N, ceiling(N/2))

    y1 <- y[S1]
    x1 <- x[S1]

    F1 <- glm(y1 ~ x1, family=binomial)
    Y2.HAT <- predict(F1, newdata = data.frame(x1 = x[S2]), type = "response")
   # Y2.HAT.2 <- F1$coef[1] +  F1$coef[2] * x[S2]

    F2 <- glm(Y2.HAT ~ x[S2], family=binomial)



