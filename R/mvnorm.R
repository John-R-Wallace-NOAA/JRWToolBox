mvnorm <-
function(n = 1, mu, Sigma, p)
{
        

	if(missing(p)) {

		p <- length(mu)
                if(missing(Sigma))
                   Sigma <- diag(p)
	} else {

		mu <- rep(0, p)
		Sigma <- diag(p)
	}

	if(!all(dim(Sigma) == c(p, p)))
		stop("incompatible arguments")
	eS <- eigen(Sigma, sym = T)
	X <- mu + eS$vectors %*% diag(sqrt(eS$values)) %*% matrix(rnorm(p * n), p)
	if(n == 1)
		drop(X)
	else t(X)
}


