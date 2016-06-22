dmulti.norm <-
function(y, mu, sigma)
{
	n <- length(y)
	if(missing(mu))
		mu <- rep(0, n)
	if(missing(sigma))
		sigma <- diag(n)
	(2 * pi)^( - length(y)/2) * det(sigma)^(-0.5) * exp(-0.5 * t(y - mu) %*% solve(sigma) %*% (y - mu))
}


